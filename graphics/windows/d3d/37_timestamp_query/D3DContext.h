#ifndef D3D_36_REFACTOR2_D3DCONTEXT_H
#define D3D_36_REFACTOR2_D3DCONTEXT_H

#include <memory>

#include <wrl/client.h>
#include <d3d12.h>
#include <dxgi1_6.h>
#include <D3D12MemAlloc.h>

#include "D3DDebug.h"

class D3DContext
{
public:
    D3DContext();
    ~D3DContext();

    Microsoft::WRL::ComPtr<IDXGIFactory7> dxgiFactory() const
    {
        return m_dxgiFactory;
    }

    Microsoft::WRL::ComPtr<ID3D12Device> device() const
    {
        return m_device;
    }

    Microsoft::WRL::ComPtr<IDXGIAdapter1> adapter() const
    {
        return m_adapter;
    }

    Microsoft::WRL::ComPtr<D3D12MA::Allocator> allocator() const
    {
        return m_allocator;
    }

private:
    void createDXGIFactory();
    void getAdapter();
    void createDevice();
    void createAllocator();

    Microsoft::WRL::ComPtr<IDXGIFactory7> m_dxgiFactory;
    Microsoft::WRL::ComPtr<ID3D12Device> m_device;
    Microsoft::WRL::ComPtr<IDXGIAdapter1> m_adapter;
    Microsoft::WRL::ComPtr<D3D12MA::Allocator> m_allocator;

    std::unique_ptr<D3DDebug> m_debug;
};


#endif //D3D_36_REFACTOR2_D3DCONTEXT_H
