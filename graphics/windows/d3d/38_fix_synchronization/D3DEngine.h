#ifndef D3D_01_HELLO_WORLD_D3DENGINE_H
#define D3D_01_HELLO_WORLD_D3DENGINE_H

#include <array>
#include <memory>

#ifndef UNICODE
#define UNICODE
#endif
#include <windows.h>

#include <d3d12.h>
#include <d3dcompiler.h>
#include <dxgi1_6.h>
#include <wrl/client.h>
#include <DirectXMath.h>
#include <D3D12MemAlloc.h>

#include "D3DContext.h"
#include "D3DDebug.h"
#include "D3DDescriptorHeap.h"
#include "Model.h"

class D3DEngine
{
public:
    explicit D3DEngine(HWND hwnd, D3DContext *context);
    ~D3DEngine();

    void cleanup();

    void render();

private:
    void createCommandResources();
    void createSwapChain(HWND hwnd);
    void createSwapChainResources();
    void createDepthResources(UINT width, UINT height);
    void createFence();

    static Microsoft::WRL::ComPtr<ID3D10Blob> compileShader(
        const wchar_t *fileName,
        const char *entryPoint,
        const char *target,
        UINT flags = D3DCOMPILE_ENABLE_STRICTNESS | D3DCOMPILE_DEBUG
    );

    void createPipelineState();
    void createViewport(HWND hwnd);

    void barrier(
        const Microsoft::WRL::ComPtr<ID3D12Resource> &resource,
        D3D12_RESOURCE_STATES beforeState,
        D3D12_RESOURCE_STATES afterState
    ) const;

    void beginFrame(UINT frameIndex);
    void recordCommands(UINT frameIndex) const;
    void endFrame(UINT frameIndex);

    void waitForFence(const Microsoft::WRL::ComPtr<ID3D12CommandQueue>& queue, UINT frameIndex);
    void executeCommand(UINT frameIndex);

    D3DContext *m_context;
    std::unique_ptr<Model> m_model;
    std::unique_ptr<DescriptorHeapManager> m_descHeapManager;

    static constexpr UINT FRAME_COUNT = 2;

    std::array<Microsoft::WRL::ComPtr<ID3D12CommandAllocator>, FRAME_COUNT> m_commandAllocators;
    Microsoft::WRL::ComPtr<ID3D12CommandQueue> m_commandQueue;
    Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> m_commandList;

    Microsoft::WRL::ComPtr<IDXGISwapChain4> m_swapchain;
    std::array<Microsoft::WRL::ComPtr<ID3D12Resource>, FRAME_COUNT> m_backBuffers;
    std::array<float, 4> m_clearColor = {1.0f, 1.0f, 1.0f, 1.0f};
    std::array<Microsoft::WRL::ComPtr<D3D12MA::Allocation>, FRAME_COUNT> m_depthBuffers;

    std::array<Microsoft::WRL::ComPtr<ID3D12Fence>, FRAME_COUNT> m_fence;
    std::array<UINT64, FRAME_COUNT> m_fenceValues = {};
    std::array<HANDLE, FRAME_COUNT> m_fenceEvents = {};

    Microsoft::WRL::ComPtr<ID3D12PipelineState> m_pipelineState;
    Microsoft::WRL::ComPtr<ID3D12RootSignature> m_rootSignature;

    D3D12_VIEWPORT m_viewport = {};
    D3D12_RECT m_scissorRect = {};
};



#endif //D3D_01_HELLO_WORLD_D3DENGINE_H
