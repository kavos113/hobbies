#ifndef D3D_36_REFACTOR2_D3DBUFFER_H
#define D3D_36_REFACTOR2_D3DBUFFER_H

#include <vector>

#include <d3d12.h>
#include <wrl/client.h>
#include <D3D12MemAlloc.h>

#include "D3DContext.h"

class D3DBuffer
{
public:
    D3DBuffer(D3DContext *context);
    ~D3DBuffer();

    void createBuffer(
        D3D12_RESOURCE_DESC resourceDesc,
        D3D12_HEAP_TYPE heapType,
        D3D12_RESOURCE_STATES initialState,
        D3D12MA::Allocation **buffer
    );

    void copyBuffer(const Microsoft::WRL::ComPtr<D3D12MA::Allocation> &srcBuffer, const Microsoft::WRL::ComPtr<D3D12MA::Allocation> &dstBuffer) const;
    void copyTexture(const Microsoft::WRL::ComPtr<D3D12MA::Allocation> &srcTexture, const Microsoft::WRL::ComPtr<D3D12MA::Allocation> &dstTexture) const;

    void executeCopy();
    void registerWaitForCopyResource(const Microsoft::WRL::ComPtr<D3D12MA::Allocation> &buffer);

private:
    void createCommandResources();

    D3DContext *m_context;

    Microsoft::WRL::ComPtr<ID3D12CommandAllocator> m_copyCommandAllocator;
    Microsoft::WRL::ComPtr<ID3D12CommandQueue> m_copyCommandQueue;
    Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> m_copyCommandList;
    Microsoft::WRL::ComPtr<ID3D12Fence> m_copyFence;
    UINT64 m_copyFenceValue = 0;
    HANDLE m_copyFenceEvent = nullptr;

    std::vector<Microsoft::WRL::ComPtr<D3D12MA::Allocation>> m_waitForCopyResources;
};


#endif //D3D_36_REFACTOR2_D3DBUFFER_H
