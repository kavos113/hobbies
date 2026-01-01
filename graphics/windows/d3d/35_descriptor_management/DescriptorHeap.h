#ifndef WIN32D2DEXAMPLES_DESCRIPTORHEAP_H
#define WIN32D2DEXAMPLES_DESCRIPTORHEAP_H

#include <d3d12.h>
#include <wrl/client.h>

#include <array>
#include <cstdint>
#include <vector>
#include <memory>

class GPUDescriptorHeap
{
public:
    struct DescriptorHandle
    {
        D3D12_CPU_DESCRIPTOR_HANDLE cpuHandle;
        D3D12_GPU_DESCRIPTOR_HANDLE gpuHandle;
    };

    GPUDescriptorHeap(const Microsoft::WRL::ComPtr<ID3D12Device> &device, D3D12_DESCRIPTOR_HEAP_TYPE type, uint32_t heapSize);
    ~GPUDescriptorHeap() = default;

    DescriptorHandle allocate(uint32_t count);
    void bind(const Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> &cmdList);

    D3D12_DESCRIPTOR_HEAP_TYPE type() const;
    uint32_t latestIndex() const;

private:
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_heap;
    Microsoft::WRL::ComPtr<ID3D12Device> m_device;
    D3D12_DESCRIPTOR_HEAP_TYPE m_descHeapType;
    uint32_t m_latestIndex = 0;

    uint32_t m_heapSize;
};

class CPUDescriptorHeap
{
public:
    CPUDescriptorHeap(const Microsoft::WRL::ComPtr<ID3D12Device> &device, D3D12_DESCRIPTOR_HEAP_TYPE type, uint32_t heapSize);
    ~CPUDescriptorHeap() = default;

    D3D12_CPU_DESCRIPTOR_HANDLE allocate(uint32_t count);
    D3D12_CPU_DESCRIPTOR_HANDLE cpuHandle(UINT index) const;

private:
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_heap;
    Microsoft::WRL::ComPtr<ID3D12Device> m_device;
    D3D12_DESCRIPTOR_HEAP_TYPE m_descHeapType;
    uint32_t m_latestIndex = 0;

    uint32_t m_heapSize;
};

class DescriptorBindingManager
{
public:
    struct BindingParameter
    {
        D3D12_DESCRIPTOR_RANGE1 range;
        D3D12_CPU_DESCRIPTOR_HANDLE handle;
    };

    DescriptorBindingManager(const Microsoft::WRL::ComPtr<ID3D12Device> &device);
    ~DescriptorBindingManager() = default;

    enum class DescriptorResourceType : uint8_t
    {
        VS_CBV = 0,
        VS_SRV = 1,
        PS_CBV = 2,
        PS_SRV = 3,
        EnumCount = 4
    };

    void registerBinding(const BindingParameter &binding, D3D12_SHADER_VISIBILITY visibility);
    void copyAndSubmit(const Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> &cmdList, GPUDescriptorHeap *gpuHeap);

    std::vector<D3D12_ROOT_PARAMETER1> rootParameter();
private:
    std::array<std::vector<BindingParameter>, static_cast<size_t>(DescriptorResourceType::EnumCount)> m_bindings;

    Microsoft::WRL::ComPtr<ID3D12Device> m_device;
};

class DescriptorHeapManager
{
public:
    DescriptorHeapManager(Microsoft::WRL::ComPtr<ID3D12Device> device);
    ~DescriptorHeapManager() = default;

    void bind(const Microsoft::WRL::ComPtr<ID3D12GraphicsCommandList> &cmdList) const;

    CPUDescriptorHeap *rtvHeap() const { return m_rtvHeap.get(); }
    CPUDescriptorHeap *dsvHeap() const { return m_dsvHeap.get(); }
    CPUDescriptorHeap *cbvHeap() const { return m_cbvHeap.get(); }
    GPUDescriptorHeap *gpuCbvHeap() const { return m_gpuCbvHeap.get(); }
    GPUDescriptorHeap *samplerHeap() const { return m_samplerHeap.get(); }
    std::vector<D3D12_ROOT_PARAMETER1> rootParameter() const { return m_cbvManager->rootParameter(); }
private:
    std::unique_ptr<CPUDescriptorHeap> m_rtvHeap;
    std::unique_ptr<CPUDescriptorHeap> m_dsvHeap;
    std::unique_ptr<CPUDescriptorHeap> m_cbvHeap;
    std::unique_ptr<GPUDescriptorHeap> m_gpuCbvHeap;
    std::unique_ptr<GPUDescriptorHeap> m_samplerHeap;
    std::unique_ptr<DescriptorBindingManager> m_cbvManager;

    static constexpr uint32_t DEFAULT_HEAP_SIZE = 131072;
    static constexpr uint32_t SAMPLER_HEAP_SIZE = 2048;
};


#endif //WIN32D2DEXAMPLES_DESCRIPTORHEAP_H