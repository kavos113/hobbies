#ifndef WIN32D2DEXAMPLES_DESCRIPTORHEAP_H
#define WIN32D2DEXAMPLES_DESCRIPTORHEAP_H

#include <d3d12.h>
#include <wrl/client.h>

#include <array>
#include <cstdint>
#include <vector>

class GPUDescriptorHeap
{
public:
    struct DescriptorHandle
    {
        D3D12_CPU_DESCRIPTOR_HANDLE cpuHandle;
        D3D12_GPU_DESCRIPTOR_HANDLE gpuHandle;
    };

    GPUDescriptorHeap(const Microsoft::WRL::ComPtr<ID3D12Device> &device, D3D12_DESCRIPTOR_HEAP_TYPE type);
    ~GPUDescriptorHeap() = default;

    DescriptorHandle allocate(uint32_t count);

    D3D12_DESCRIPTOR_HEAP_TYPE type() const;
    uint32_t latestIndex() const;

private:
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_heap;
    Microsoft::WRL::ComPtr<ID3D12Device> m_device;
    D3D12_DESCRIPTOR_HEAP_TYPE m_descHeapType;
    uint32_t m_latestIndex = 0;

    static constexpr uint32_t HEAP_SIZE = 131072;
};

class CPUDescriptorHeap
{
public:
    CPUDescriptorHeap(const Microsoft::WRL::ComPtr<ID3D12Device> &device, D3D12_DESCRIPTOR_HEAP_TYPE type);
    ~CPUDescriptorHeap() = default;

    D3D12_CPU_DESCRIPTOR_HANDLE allocate(uint32_t count);

private:
    Microsoft::WRL::ComPtr<ID3D12DescriptorHeap> m_heap;
    Microsoft::WRL::ComPtr<ID3D12Device> m_device;
    D3D12_DESCRIPTOR_HEAP_TYPE m_descHeapType;
    uint32_t m_latestIndex = 0;

    static constexpr uint32_t HEAP_SIZE = 131072;
};

class DescriptorBindingManager
{
public:
    struct BindingParameter
    {
        D3D12_DESCRIPTOR_RANGE1 range;
        D3D12_CPU_DESCRIPTOR_HANDLE handle;
    };

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


#endif //WIN32D2DEXAMPLES_DESCRIPTORHEAP_H