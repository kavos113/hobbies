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
    : m_bindings({}), m_device(device), m_nullCbv(), m_nullSrv(),
    DESCRIPTOR_RANGES({
        D3D12_DESCRIPTOR_RANGE1{
            .RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_CBV,
            .NumDescriptors = REGISTER_COUNT[VS_CBV],
            .BaseShaderRegister = 0,
            .RegisterSpace = 0,
            .Flags = D3D12_DESCRIPTOR_RANGE_FLAG_DESCRIPTORS_VOLATILE,
            .OffsetInDescriptorsFromTableStart = 0
        },
        D3D12_DESCRIPTOR_RANGE1{
            .RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV,
            .NumDescriptors = REGISTER_COUNT[VS_SRV],
            .BaseShaderRegister = 0,
            .RegisterSpace = 0,
            .Flags = D3D12_DESCRIPTOR_RANGE_FLAG_DESCRIPTORS_VOLATILE,
            .OffsetInDescriptorsFromTableStart = 0
        },
        D3D12_DESCRIPTOR_RANGE1{
            .RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_CBV,
            .NumDescriptors = REGISTER_COUNT[PS_CBV],
            .BaseShaderRegister = 0,
            .RegisterSpace = 0,
            .Flags = D3D12_DESCRIPTOR_RANGE_FLAG_DESCRIPTORS_VOLATILE,
            .OffsetInDescriptorsFromTableStart = 0
        },
        D3D12_DESCRIPTOR_RANGE1{
            .RangeType = D3D12_DESCRIPTOR_RANGE_TYPE_SRV,
            .NumDescriptors = REGISTER_COUNT[PS_SRV],
            .BaseShaderRegister = 0,
            .RegisterSpace = 0,
            .Flags = D3D12_DESCRIPTOR_RANGE_FLAG_DESCRIPTORS_VOLATILE,
            .OffsetInDescriptorsFromTableStart = 0
        },
    })
{
    m_bindings[VS_CBV].bindings.resize(REGISTER_COUNT[VS_CBV]);
    m_bindings[VS_SRV].bindings.resize(REGISTER_COUNT[VS_SRV]);
    m_bindings[PS_CBV].bindings.resize(REGISTER_COUNT[PS_CBV]);
    m_bindings[PS_SRV].bindings.resize(REGISTER_COUNT[PS_SRV]);
}

void DescriptorBindingManager::setHandle(
    D3D12_CPU_DESCRIPTOR_HANDLE handle,
    UINT registerIndex,
    DescriptorResourceType type
)
{
    m_bindings[type].bindings[registerIndex] = {handle, 1};
    m_bindings[type].isDirty = true;
}

void DescriptorBindingManager::setHandleArray(
    D3D12_CPU_DESCRIPTOR_HANDLE startHandle,
    UINT count,
    UINT startRegisterIndex,
    DescriptorResourceType type
)
{
    m_bindings[type].bindings[startRegisterIndex] = {startHandle, count};
    m_bindings[type].isDirty = true;
}

void DescriptorBindingManager::copyAndSubmit(const Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> &cmdList, GPUDescriptorHeap *gpuHeap)
{
    std::vector<D3D12_CPU_DESCRIPTOR_HANDLE> srcHandles;
    std::vector<UINT> srcSizes;
    UINT dstSize = 0;

    for (uint8_t i = 0; i < m_bindings.size(); i++)
    {
        if (!m_bindings[i].isDirty)
        {
            continue;
        }

        for (uint32_t j = 0; j < m_bindings[i].bindings.size(); )
        {
            if (m_bindings[i].bindings[j].isValid())
            {
                srcHandles.push_back(m_bindings[i].bindings[j].handle);
                srcSizes.push_back(m_bindings[i].bindings[j].count);

                j += m_bindings[i].bindings[j].count;
            }
            else
            {
                switch (i)
                {
                case VS_CBV:
                case PS_CBV:
                    srcHandles.push_back(m_nullCbv);
                    break;
                case VS_SRV:
                case PS_SRV:
                    srcHandles.push_back(m_nullSrv);
                    break;
                default:
                    break;
                }
                srcSizes.push_back(1);

                j++;
            }
        }

        dstSize += REGISTER_COUNT[i];
    }

    std::vector<D3D12_GPU_DESCRIPTOR_HANDLE> handles;
    handles.reserve(m_bindings.size());
    UINT incrementSize = m_device->GetDescriptorHandleIncrementSize(gpuHeap->type());

    if (dstSize > 0)
    {
        GPUDescriptorHeap::DescriptorHandle dstHandle = gpuHeap->allocate(dstSize);
        m_device->CopyDescriptors(
            1, &dstHandle.cpuHandle, &dstSize,
            srcHandles.size(), srcHandles.data(), srcSizes.data(),
            D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV
        );

        D3D12_GPU_DESCRIPTOR_HANDLE gpuHandle = dstHandle.gpuHandle;

        for (uint8_t i = 0; i < ResourceTypeCount; i++)
        {
            if (m_bindings[i].isDirty)
            {
                handles.push_back(gpuHandle);
                m_bindings[i].startGpuHandle = gpuHandle;

                gpuHandle.ptr += REGISTER_COUNT[i] * incrementSize;
            }
            else
            {
                handles.push_back(m_bindings[i].startGpuHandle);
            }
        }
    }
    else
    {
        for (const auto &binding : m_bindings)
        {
            handles.push_back(binding.startGpuHandle);
        }
    }


    for (uint8_t i = 0; i < ResourceTypeCount; i++)
    {
        cmdList->SetGraphicsRootDescriptorTable(i, handles[i]);
    }
}

std::vector<D3D12_ROOT_PARAMETER1> DescriptorBindingManager::rootParameter() const
{
    return {
        D3D12_ROOT_PARAMETER1{
            .ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
            .DescriptorTable = {
                .NumDescriptorRanges = 1,
                .pDescriptorRanges = &DESCRIPTOR_RANGES[VS_CBV]
            },
            .ShaderVisibility = D3D12_SHADER_VISIBILITY_VERTEX
        },
        D3D12_ROOT_PARAMETER1{
            .ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
            .DescriptorTable = {
                .NumDescriptorRanges = 1,
                .pDescriptorRanges = &DESCRIPTOR_RANGES[VS_SRV]
            },
            .ShaderVisibility = D3D12_SHADER_VISIBILITY_VERTEX
        },
        D3D12_ROOT_PARAMETER1{
            .ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
            .DescriptorTable = {
                .NumDescriptorRanges = 1,
                .pDescriptorRanges = &DESCRIPTOR_RANGES[PS_CBV]
            },
            .ShaderVisibility = D3D12_SHADER_VISIBILITY_PIXEL
        },
        D3D12_ROOT_PARAMETER1{
            .ParameterType = D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
            .DescriptorTable = {
                .NumDescriptorRanges = 1,
                .pDescriptorRanges = &DESCRIPTOR_RANGES[PS_SRV]
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

    D3D12_CPU_DESCRIPTOR_HANDLE nullCbvHandle = m_cbvHeap->allocate(1);
    D3D12_CPU_DESCRIPTOR_HANDLE nullSrvHandle = m_cbvHeap->allocate(1);

    D3D12_CONSTANT_BUFFER_VIEW_DESC nullCbvDesc = {
        .BufferLocation = 0,
        .SizeInBytes = 0
    };
    device->CreateConstantBufferView(&nullCbvDesc, nullCbvHandle);

    D3D12_SHADER_RESOURCE_VIEW_DESC nullSrvDesc = {
        .Format = DXGI_FORMAT_B8G8R8X8_UNORM,
        .ViewDimension = D3D12_SRV_DIMENSION_TEXTURE2D,
        .Shader4ComponentMapping = D3D12_DEFAULT_SHADER_4_COMPONENT_MAPPING
    };
    device->CreateShaderResourceView(nullptr, &nullSrvDesc, nullSrvHandle);

    m_cbvManager->setNullCbv(nullCbvHandle);
    m_cbvManager->setNullSrv(nullSrvHandle);
}

void DescriptorHeapManager::bind(const Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> &cmdList) const
{
    m_gpuCbvHeap->bind(cmdList);
    m_cbvManager->copyAndSubmit(cmdList, m_gpuCbvHeap.get());
}
