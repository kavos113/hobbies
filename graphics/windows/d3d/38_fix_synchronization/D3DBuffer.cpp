#include "D3DBuffer.h"

#include <iostream>
#include <array>

#include "D3DContext.h"

D3DBuffer::D3DBuffer(D3DContext* context)
    : m_context(context)
{
    createCommandResources();
}

D3DBuffer::~D3DBuffer()
{
    CloseHandle(m_copyFenceEvent);
}

void D3DBuffer::createBuffer(
    D3D12_RESOURCE_DESC resourceDesc,
    D3D12_HEAP_TYPE heapType,
    D3D12_RESOURCE_STATES initialState,
    D3D12MA::Allocation** buffer
)
{
    D3D12MA::ALLOCATION_DESC allocDesc = {};
    allocDesc.HeapType = heapType;

    HRESULT hr = m_context->allocator()->CreateResource(
        &allocDesc,
        &resourceDesc,
        initialState,
        nullptr,
        buffer,
        IID_NULL,
        nullptr
    );
    if (FAILED(hr))
    {
        std::cerr << "failed to create buffer" << std::endl;
        return;
    }
}

void D3DBuffer::copyBuffer(
    const Microsoft::WRL::ComPtr<D3D12MA::Allocation>& srcBuffer,
    const Microsoft::WRL::ComPtr<D3D12MA::Allocation>& dstBuffer
) const
{
    m_copyCommandList->CopyResource(dstBuffer->GetResource(), srcBuffer->GetResource());
}

void D3DBuffer::copyTexture(
    const Microsoft::WRL::ComPtr<D3D12MA::Allocation>& srcTexture,
    const Microsoft::WRL::ComPtr<D3D12MA::Allocation>& dstTexture
) const
{
    D3D12_RESOURCE_DESC resDesc = dstTexture->GetResource()->GetDesc();

    D3D12_PLACED_SUBRESOURCE_FOOTPRINT layout = {};
    UINT64 requiredSize = 0;
    m_context->device()->GetCopyableFootprints(
        &resDesc,
        0,
        1,
        0,
        &layout,
        nullptr,
        nullptr,
        &requiredSize
    );

    D3D12_TEXTURE_COPY_LOCATION srcLocation = {
        .pResource = srcTexture->GetResource(),
        .Type = D3D12_TEXTURE_COPY_TYPE_PLACED_FOOTPRINT,
        .PlacedFootprint = layout
    };

    D3D12_TEXTURE_COPY_LOCATION dstLocation = {
        .pResource = dstTexture->GetResource(),
        .Type = D3D12_TEXTURE_COPY_TYPE_SUBRESOURCE_INDEX,
        .SubresourceIndex = 0
    };

    m_copyCommandList->CopyTextureRegion(
        &dstLocation,
        0, 0, 0,
        &srcLocation,
        nullptr
    );
}

void D3DBuffer::executeCopy()
{
    HRESULT hr = m_copyCommandList->Close();
    if (FAILED(hr))
    {
        std::cerr << "failed to close copy command list" << std::endl;
        return;
    }

    std::array<ID3D12CommandList *, 1> commandLists = { m_copyCommandList.Get() };
    m_copyCommandQueue->ExecuteCommandLists(commandLists.size(), commandLists.data());

    m_copyFenceValue++;
    hr = m_copyCommandQueue->Signal(m_copyFence.Get(), m_copyFenceValue);
    if (FAILED(hr))
    {
        std::cerr << "failed to signal copy command queue" << std::endl;
        return;
    }

    if (m_copyFence->GetCompletedValue() < m_copyFenceValue)
    {
        hr = m_copyFence->SetEventOnCompletion(m_copyFenceValue, m_copyFenceEvent);
        if (FAILED(hr))
        {
            std::cerr << "failed to set event on copy fence completion" << std::endl;
            return;
        }

        WaitForSingleObject(m_copyFenceEvent, INFINITE);
    }

    hr = m_copyCommandAllocator->Reset();
    if (FAILED(hr))
    {
        std::cerr << "failed to reset copy command allocator" << std::endl;
        return;
    }

    hr = m_copyCommandList->Reset(m_copyCommandAllocator.Get(), nullptr);
    if (FAILED(hr))
    {
        std::cerr << "failed to reset copy command list" << std::endl;
        return;
    }

    m_waitForCopyResources.clear();
}

void D3DBuffer::registerWaitForCopyResource(const Microsoft::WRL::ComPtr<D3D12MA::Allocation>& buffer)
{
    m_waitForCopyResources.push_back(buffer);
}

void D3DBuffer::createCommandResources()
{
    HRESULT hr = m_context->device()->CreateCommandAllocator(
        D3D12_COMMAND_LIST_TYPE_COPY,
        IID_PPV_ARGS(&m_copyCommandAllocator)
    );
    if (FAILED(hr))
    {
        std::cerr << "failed to create copy command allocator" << std::endl;
        return;
    }

    hr = m_context->device()->CreateCommandList(
        0,
        D3D12_COMMAND_LIST_TYPE_COPY,
        m_copyCommandAllocator.Get(),
        nullptr,
        IID_PPV_ARGS(&m_copyCommandList)
    );
    if (FAILED(hr))
    {
        std::cerr << "failed to create copy command list" << std::endl;
        return;
    }

    D3D12_COMMAND_QUEUE_DESC queueDesc = {
        .Type = D3D12_COMMAND_LIST_TYPE_COPY,
        .Priority = D3D12_COMMAND_QUEUE_PRIORITY_NORMAL,
        .Flags = D3D12_COMMAND_QUEUE_FLAG_NONE,
        .NodeMask = 0
    };
    hr = m_context->device()->CreateCommandQueue(
        &queueDesc,
        IID_PPV_ARGS(&m_copyCommandQueue)
    );
    if (FAILED(hr))
    {
        std::cerr << "failed to create copy command queue" << std::endl;
        return;
    }

    m_copyFenceValue = 0;
    m_copyFenceEvent = CreateEvent(nullptr, FALSE, FALSE, nullptr);
    if (!m_copyFenceEvent)
    {
        std::cerr << "failed to create copy fence event" << std::endl;
        return;
    }

    hr = m_context->device()->CreateFence(
        m_copyFenceValue,
        D3D12_FENCE_FLAG_NONE,
        IID_PPV_ARGS(&m_copyFence)
    );
    if (FAILED(hr))
    {
        std::cerr << "failed to create copy fence" << std::endl;
        return;
    }
}
