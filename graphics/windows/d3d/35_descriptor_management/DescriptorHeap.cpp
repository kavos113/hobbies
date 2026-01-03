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

void GPUDescriptorHeap::bind(const Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> &cmdList)
{
    cmdList->SetDescriptorHeaps(1, m_heap.GetAddressOf());
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
    handle.ptr += m_latestIndex * m_device->GetDescriptorHandleIncrementSize(m_descHeapType);

    m_latestIndex += count;
    return handle;
}

D3D12_CPU_DESCRIPTOR_HANDLE CPUDescriptorHeap::cpuHandle(const UINT index) const
{
    D3D12_CPU_DESCRIPTOR_HANDLE handle = m_heap->GetCPUDescriptorHandleForHeapStart();
    handle.ptr += index * m_device->GetDescriptorHandleIncrementSize(m_descHeapType);

    return handle;
}

DescriptorBindingManager::DescriptorBindingManager(const Microsoft::WRL::ComPtr<ID3D12Device> &device)
    : m_device(device), m_bindings({})
{
    m_bindings[VS_CBV].reserve(REGISTER_COUNT[VS_CBV]);
    m_bindings[VS_SRV].reserve(REGISTER_COUNT[VS_SRV]);
    m_bindings[PS_CBV].reserve(REGISTER_COUNT[PS_CBV]);
    m_bindings[PS_SRV].reserve(REGISTER_COUNT[PS_SRV]);
}

void DescriptorBindingManager::setHandle(
    D3D12_CPU_DESCRIPTOR_HANDLE handle,
    UINT registerIndex,
    DescriptorResourceType type
)
{
    m_bindings[type][registerIndex] = {handle, 1};
}

void DescriptorBindingManager::setHandleArray(
    D3D12_CPU_DESCRIPTOR_HANDLE startHandle,
    UINT count,
    UINT startRegisterIndex,
    DescriptorResourceType type
)
{
    m_bindings[type][startRegisterIndex] = {startHandle, count};
}

void DescriptorBindingManager::copyAndSubmit(const Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> &cmdList, GPUDescriptorHeap *gpuHeap) const
{
    std::vector<D3D12_CPU_DESCRIPTOR_HANDLE> srcHandles;
    std::vector<UINT> srcSizes;

    std::array<uint32_t, static_cast<size_t>(ResourceTypeCount)> counts = {};

    for (uint8_t i = 0; i < m_bindings.size(); i++)
    {
        for (uint32_t j = 0; j < m_bindings[i].size(); )
        {
            srcHandles.push_back(m_bindings[i][j].handle);
            srcSizes.push_back(m_bindings[i][j].count);

            counts[i] += m_bindings[i][j].count;

            j += m_bindings[i][j].count;
        }
    }

    UINT dstSize = std::accumulate(srcSizes.begin(), srcSizes.end(), 0);
    GPUDescriptorHeap::DescriptorHandle dstHandle = gpuHeap->allocate(dstSize);

    m_device->CopyDescriptors(
        1, &dstHandle.cpuHandle, &dstSize,
        srcHandles.size(), srcHandles.data(), srcSizes.data(),
        D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV
    );

    UINT incrementSize = m_device->GetDescriptorHandleIncrementSize(gpuHeap->type());
    uint32_t firstIndex = gpuHeap->latestIndex() - dstSize;

    D3D12_GPU_DESCRIPTOR_HANDLE gpuHandle = dstHandle.gpuHandle;
    gpuHandle.ptr += firstIndex * incrementSize;
    for (uint8_t i = 0; i < static_cast<uint8_t>(ResourceTypeCount); i++)
    {
        cmdList->SetGraphicsRootDescriptorTable(i, gpuHandle);
        gpuHandle.ptr += counts[i] * incrementSize;
    }
}

std::vector<D3D12_ROOT_PARAMETER1> DescriptorBindingManager::rootParameter() const
{
    D3D12_DESCRIPTOR_RANGE1 vsCbvRange = {
        .RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_CBV,
        .NumDescriptors = REGISTER_COUNT[VS_CBV],
        .BaseShaderRegister = 0,
        .RegisterSpace = 0,
        .Flags = D3D12_DESCRIPTOR_RANGE_FLAG_NONE,
        .OffsetInDescriptorsFromTableStart = 0
    };
    D3D12_DESCRIPTOR_RANGE1 vsSrvRange = {
        .RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV,
        .NumDescriptors = REGISTER_COUNT[VS_SRV],
        .BaseShaderRegister = 0,
        .RegisterSpace = 0,
        .Flags = D3D12_DESCRIPTOR_RANGE_FLAG_NONE,
        .OffsetInDescriptorsFromTableStart = 0
    };
    D3D12_DESCRIPTOR_RANGE1 psCbvRange = {
        .RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_CBV,
        .NumDescriptors = REGISTER_COUNT[PS_CBV],
        .BaseShaderRegister = 0,
        .RegisterSpace = 0,
        .Flags = D3D12_DESCRIPTOR_RANGE_FLAG_NONE,
        .OffsetInDescriptorsFromTableStart = 0
    };
    D3D12_DESCRIPTOR_RANGE1 psSrvRange = {
        .RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV,
        .NumDescriptors = REGISTER_COUNT[PS_SRV],
        .BaseShaderRegister = 0,
        .RegisterSpace = 0,
        .Flags = D3D12_DESCRIPTOR_RANGE_FLAG_NONE,
        .OffsetInDescriptorsFromTableStart = 0
    };

    return {
        D3D12_ROOT_PARAMETER1{
            .ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
            .DescriptorTable = {
                .NumDescriptorRanges = 1,
                .pDescriptorRanges = &vsCbvRange
            },
            .ShaderVisibility = D3D12_SHADER_VISIBILITY_VERTEX
        },
        D3D12_ROOT_PARAMETER1{
            .ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
            .DescriptorTable = {
                .NumDescriptorRanges = 1,
                .pDescriptorRanges = &vsSrvRange
            },
            .ShaderVisibility = D3D12_SHADER_VISIBILITY_VERTEX
        },
        D3D12_ROOT_PARAMETER1{
            .ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
            .DescriptorTable = {
                .NumDescriptorRanges = 1,
                .pDescriptorRanges = &psCbvRange
            },
            .ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL
        },
        D3D12_ROOT_PARAMETER1{
            .ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
            .DescriptorTable = {
                .NumDescriptorRanges = 1,
                .pDescriptorRanges = &psSrvRange
            },
            .ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL
        }
    };
}

void DescriptorBindingManager::setNullCbv(D3D12_CPU_DESCRIPTOR_HANDLE handle)
{
    m_nullCbv = handle;
}

void DescriptorBindingManager::setNullSrv(D3D12_CPU_DESCRIPTOR_HANDLE handle)
{
    m_nullSrv = handle;
}

DescriptorHeapManager::DescriptorHeapManager(Microsoft::WRL::ComPtr<ID3D12Device> device)
{
    m_rtvHeap = std::make_unique<CPUDescriptorHeap>(device, D3D12_DESCRIPTOR_HEAP_TYPE_RTV, DEFAULT_HEAP_SIZE);
    m_dsvHeap = std::make_unique<CPUDescriptorHeap>(device, D3D12_DESCRIPTOR_HEAP_TYPE_DSV, DEFAULT_HEAP_SIZE);
    m_cbvHeap = std::make_unique<CPUDescriptorHeap>(device, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, DEFAULT_HEAP_SIZE);
    m_gpuCbvHeap = std::make_unique<GPUDescriptorHeap>(device, D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV, DEFAULT_HEAP_SIZE);
    m_samplerHeap = std::make_unique<GPUDescriptorHeap>(device, D3D12_DESCRIPTOR_HEAP_TYPE_SAMPLER, SAMPLER_HEAP_SIZE);
    m_cbvManager = std::make_unique<DescriptorBindingManager>(device);
}

void DescriptorHeapManager::bind(const Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> &cmdList) const
{
    m_gpuCbvHeap->bind(cmdList);
    m_cbvManager->copyAndSubmit(cmdList, m_gpuCbvHeap.get());
}
