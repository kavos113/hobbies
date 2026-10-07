#ifndef D3D_36_REFACTOR2_D3DBUFFER_H
#define D3D_36_REFACTOR2_D3DBUFFER_H

#include <vector>

#include <d3d12.h>
#include <wrl/client.h>
#include <D3D12MemAlloc.h>

#define AlignCBuffer(x) (((x) + 0xff) & ~0xff)

class D3DContext;

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

    static D3D12_RESOURCE_DESC ResourceDesc_Buffer(UINT64 size)
    {
        return D3D12_RESOURCE_DESC{
            .Dimension = D3D12_RESOURCE_DIMENSION_BUFFER,
            .Alignment = 0,
            .Width = size,
            .Height = 1,
            .DepthOrArraySize = 1,
            .MipLevels = 1,
            .Format = DXGI_FORMAT_UNKNOWN,
            .SampleDesc = {1, 0},
            .Layout = D3D12_TEXTURE_LAYOUT_ROW_MAJOR,
            .Flags = D3D12_RESOURCE_FLAG_NONE
        };
    }

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
