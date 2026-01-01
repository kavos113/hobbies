#include "DescriptorHeap.h"

#include <ranges>
#include <numeric>
#include <iostream>

GPUDescriptorHeap::GPUDescriptorHeap(const Microsoft::WRL::ComPtr<ID3D12Device> &device, const D3D12_DESCRIPTOR_HEAP_TYPE type, uint32_t heapSize)
    : m_device(device), m_descHeapType(type), m_heapSize(heapSize)
{
    D3D12_DESCRIPTOR_HEAP_DESC heapDesc = {
        .Type = type,
        .NumDescriptors = heapSize,
        .Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
        .NodeMask = 0
    };
    HRESULT hr = device->CreateDescriptorHeap(
        &heapDesc,
        IID_PPV_ARGS(&m_heap)
    );
    if (FAILED(hr))
    {
        std::cerr << "failed to create descriptor heap" << std::endl;
        return;
    }
}

GPUDescriptorHeap::DescriptorHandle GPUDescriptorHeap::allocate(const uint32_t count)
{
    D3D12_CPU_DESCRIPTOR_HANDLE cpuHandle = m_heap->GetCPUDescriptorHandleForHeapStart();
    D3D12_GPU_DESCRIPTOR_HANDLE gpuHandle = m_heap->GetGPUDescriptorHandleForHeapStart();
    cpuHandle.ptr += m_latestIndex * m_device->GetDescriptorHandleIncrementSize(m_descHeapType);
    gpuHandle.ptr += m_latestIndex * m_device->GetDescriptorHandleIncrementSize(m_descHeapType);

    m_latestIndex += count;
    return {cpuHandle, gpuHandle};
}

uint32_t GPUDescriptorHeap::latestIndex() const
{
    return m_latestIndex;
}

D3D12_DESCRIPTOR_HEAP_TYPE GPUDescriptorHeap::type() const
{
    return m_descHeapType;
}

CPUDescriptorHeap::CPUDescriptorHeap(const Microsoft::WRL::ComPtr<ID3D12Device> &device, const D3D12_DESCRIPTOR_HEAP_TYPE type, uint32_t heapSize)
    : m_device(device), m_descHeapType(type), m_heapSize(heapSize)
{
    D3D12_DESCRIPTOR_HEAP_DESC heapDesc = {
        .Type = type,
        .NumDescriptors = heapSize,
        .Flags = D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
        .NodeMask = 0
    };
    HRESULT hr = device->CreateDescriptorHeap(
        &heapDesc,
        IID_PPV_ARGS(&m_heap)
    );
    if (FAILED(hr))
    {
        std::cerr << "failed to create descriptor heap" << std::endl;
        return;
    }
}

D3D12_CPU_DESCRIPTOR_HANDLE CPUDescriptorHeap::allocate(const uint32_t count)
{
    D3D12_CPU_DESCRIPTOR_HANDLE handle = m_heap->GetCPUDescriptorHandleForHeapStart();
    handle.ptr += m_latestIndex * m_device->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);

    m_latestIndex += count;
    return handle;
}

DescriptorBindingManager::DescriptorBindingManager(const Microsoft::WRL::ComPtr<ID3D12Device> &device)
    : m_device(device), m_bindings({})
{
}

void DescriptorBindingManager::registerBinding(const BindingParameter &binding, const D3D12_SHADER_VISIBILITY visibility)
{
    if (visibility == D3D12_SHADER_VISIBILITY_VERTEX)
    {
        if (binding.range.RangeType == D3D12_DESCRIPTOR_RANGE_TYPE_CBV)
        {
            m_bindings[static_cast<uint8_t>(DescriptorResourceType::VS_CBV)].push_back(binding);
        }
        else if (binding.range.RangeType == D3D12_DESCRIPTOR_RANGE_TYPE_SRV)
        {
            m_bindings[static_cast<uint8_t>(DescriptorResourceType::VS_SRV)].push_back(binding);
        }
    }
    else if (visibility == D3D12_SHADER_VISIBILITY_PIXEL)
    {
        if (binding.range.RangeType == D3D12_DESCRIPTOR_RANGE_TYPE_CBV)
        {
            m_bindings[static_cast<uint8_t>(DescriptorResourceType::PS_CBV)].push_back(binding);
        }
        else if (binding.range.RangeType == D3D12_DESCRIPTOR_RANGE_TYPE_SRV)
        {
            m_bindings[static_cast<uint8_t>(DescriptorResourceType::PS_SRV)].push_back(binding);
        }
    }
    else if (visibility == D3D12_SHADER_VISIBILITY_ALL)
    {
        if (binding.range.RangeType == D3D12_DESCRIPTOR_RANGE_TYPE_CBV)
        {
            m_bindings[static_cast<uint8_t>(DescriptorResourceType::VS_CBV)].push_back(binding);
            m_bindings[static_cast<uint8_t>(DescriptorResourceType::PS_CBV)].push_back(binding);
        }
        else if (binding.range.RangeType == D3D12_DESCRIPTOR_RANGE_TYPE_SRV)
        {
            m_bindings[static_cast<uint8_t>(DescriptorResourceType::VS_SRV)].push_back(binding);
            m_bindings[static_cast<uint8_t>(DescriptorResourceType::PS_SRV)].push_back(binding);
        }
    }
}

void DescriptorBindingManager::copyAndSubmit(const Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> &cmdList, GPUDescriptorHeap *gpuHeap)
{
    auto srcHandles = m_bindings
        | std::views::join
        | std::views::transform(&BindingParameter::handle)
        | std::ranges::to<std::vector<D3D12_CPU_DESCRIPTOR_HANDLE>>();
    auto srcSizes = m_bindings
        | std::views::join
        | std::views::transform(&BindingParameter::range)
        | std::views::transform(&D3D12_DESCRIPTOR_RANGE1::NumDescriptors)
        | std::ranges::to<std::vector<UINT>>();

    UINT dstSize = std::accumulate(srcSizes.begin(), srcSizes.end(), 0);
    GPUDescriptorHeap::DescriptorHandle dstHandle = gpuHeap->allocate(dstSize);

    uint32_t firstIndex = gpuHeap->latestIndex() - dstSize;
    std::array<uint32_t, static_cast<size_t>(DescriptorResourceType::EnumCount)> counts = {};
    for (uint8_t i = 0; i < static_cast<uint8_t>(DescriptorResourceType::EnumCount) - 1; i++)
    {
        for (auto binding : m_bindings[i])
        {
            counts[i] += binding.range.NumDescriptors;
        }
    }

    m_device->CopyDescriptors(
        1, &dstHandle.cpuHandle, &dstSize,
        srcHandles.size(), srcHandles.data(), srcSizes.data(),
        D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV
    );

    UINT incrementSize = m_device->GetDescriptorHandleIncrementSize(gpuHeap->type());

    D3D12_GPU_DESCRIPTOR_HANDLE gpuHandle = dstHandle.gpuHandle;
    gpuHandle.ptr += firstIndex * incrementSize;
    for (uint8_t i = 0; i < static_cast<uint8_t>(DescriptorResourceType::EnumCount); i++)
    {
        cmdList->SetGraphicsRootDescriptorTable(i, gpuHandle);
        gpuHandle.ptr += counts[i] * incrementSize;
    }
}

std::vector<D3D12_ROOT_PARAMETER1> DescriptorBindingManager::rootParameter()
{
    auto vsCbvRanges = m_bindings[static_cast<uint8_t>(DescriptorResourceType::VS_CBV)]
        | std::views::transform(&BindingParameter::range)
        | std::ranges::to<std::vector<D3D12_DESCRIPTOR_RANGE1>>();

    auto vsSrvRanges = m_bindings[static_cast<uint8_t>(DescriptorResourceType::VS_SRV)]
        | std::views::transform(&BindingParameter::range)
        | std::ranges::to<std::vector<D3D12_DESCRIPTOR_RANGE1>>();

    auto psCbvRanges = m_bindings[static_cast<uint8_t>(DescriptorResourceType::PS_CBV)]
        | std::views::transform(&BindingParameter::range)
        | std::ranges::to<std::vector<D3D12_DESCRIPTOR_RANGE1>>();

    auto psSrvRanges = m_bindings[static_cast<uint8_t>(DescriptorResourceType::PS_SRV)]
        | std::views::transform(&BindingParameter::range)
        | std::ranges::to<std::vector<D3D12_DESCRIPTOR_RANGE1>>();

    return {
        D3D12_ROOT_PARAMETER1{
            .ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
            .DescriptorTable = {
                .NumDescriptorRanges = static_cast<UINT>(vsCbvRanges.size()),
                .pDescriptorRanges = vsCbvRanges.data()
            },
            .ShaderVisibility = D3D12_SHADER_VISIBILITY_VERTEX
        },
        D3D12_ROOT_PARAMETER1{
            .ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
            .DescriptorTable = {
                .NumDescriptorRanges = static_cast<UINT>(vsSrvRanges.size()),
                .pDescriptorRanges = vsSrvRanges.data()
            },
            .ShaderVisibility = D3D12_SHADER_VISIBILITY_VERTEX
        },
        D3D12_ROOT_PARAMETER1{
            .ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
            .DescriptorTable = {
                .NumDescriptorRanges = static_cast<UINT>(psCbvRanges.size()),
                .pDescriptorRanges = psCbvRanges.data()
            },
            .ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL
        },
        D3D12_ROOT_PARAMETER1{
            .ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
            .DescriptorTable = {
                .NumDescriptorRanges = static_cast<UINT>(psSrvRanges.size()),
                .pDescriptorRanges = psSrvRanges.data()
            },
            .ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL
        }
    };
}

void DescriptorHeapManager::init(Microsoft::WRL::ComPtr<ID3D12Device> device)
{
    m_rtvHeap = std::make_unique<CPUDescriptorHeap>(device, D3D12_DESCRIPTOR_HEAP_TYPE_RTV, DEFAULT_HEAP_SIZE);
    m_dsvHeap = std::make_unique<CPUDescriptorHeap>(device, D3D12_DESCRIPTOR_HEAP_TYPE_DSV, DEFAULT_HEAP_SIZE);
    m_cbvHeap = std::make_unique<CPUDescriptorHeap>(device, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, DEFAULT_HEAP_SIZE);
    m_gpuCbvHeap = std::make_unique<GPUDescriptorHeap>(device, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, DEFAULT_HEAP_SIZE);
    m_samplerHeap = std::make_unique<GPUDescriptorHeap>(device, D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER, SAMPLER_HEAP_SIZE);
    m_cbvManager = std::make_unique<DescriptorBindingManager>(device);
}
