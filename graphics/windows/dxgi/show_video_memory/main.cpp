#include <dxgi1_6.h>
#include <wrl/client.h>

#include <iostream>


int main()
{
    Microsoft::WRL::ComPtr<IDXGIFactory7> factory;
    HRESULT hr = CreateDXGIFactory2(DXGI_CREATE_FACTORY_DEBUG, IID_PPV_ARGS(&factory));
    if (FAILED(hr))
    {
        std::cerr << "Failed to create DXGI Factory." << std::endl;
        exit(1);
    }

    Microsoft::WRL::ComPtr<IDXGIAdapter4> adapter;

    UINT adapterIndex = 0;

    for (
        UINT i = 0;
        factory->EnumAdapterByGpuPreference(
            i,
            DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE,
            IID_PPV_ARGS(&adapter)
        ) != DXGI_ERROR_NOT_FOUND;
        ++i
    )
    {
        DXGI_ADAPTER_DESC1 desc;
        hr = adapter->GetDesc1(&desc);
        if (FAILED(hr))
        {
            std::cerr << "Failed to get adapter description." << std::endl;
            continue;
        }

        std::wcout << L"Adapter " << i << L"\n"
                        << desc.Description << L"\n"
                        << L"Video Memory: " << desc.DedicatedVideoMemory / (1024 * 1024) << L" MB\n"
                        << L"System Memory: " << desc.DedicatedSystemMemory / (1024 * 1024) << L" MB\n"
                        << L"Shared Memory: " << desc.SharedSystemMemory / (1024 * 1024) << L" MB\n" << std::endl;
    }

    factory->EnumAdapterByGpuPreference(
        adapterIndex,
        DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE,
        IID_PPV_ARGS(&adapter)
    );
    if (FAILED(hr) && hr != DXGI_ERROR_NOT_FOUND)
    {
        std::cerr << "Failed to enumerate adapter by GPU preference." << std::endl;
        exit(1);
    }

    DXGI_QUERY_VIDEO_MEMORY_INFO memoryInfo;
    hr = adapter->QueryVideoMemoryInfo(0, DXGI_MEMORY_SEGMENT_GROUP_LOCAL, &memoryInfo);
    if (FAILED(hr))
    {
        std::cerr << "Failed to query video memory info." << std::endl;
        exit(1);
    }
    std::cout << "Budget: " << memoryInfo.Budget / (1024 * 1024) << " MB\n"
              << "CurrentUsage: " << memoryInfo.CurrentUsage / (1024 * 1024) << " MB\n"
              << "AvailableForReservation: " << memoryInfo.AvailableForReservation / (1024 * 1024) << " MB\n"
              << "CurrentReservation: " << memoryInfo.CurrentReservation / (1024 * 1024) << " MB\n" << std::endl;


    hr = adapter->QueryVideoMemoryInfo(0, DXGI_MEMORY_SEGMENT_GROUP_NON_LOCAL, &memoryInfo);
    if (FAILED(hr))
    {
        std::cerr << "Failed to query video memory info." << std::endl;
        exit(1);
    }
    std::cout << "Budget: " << memoryInfo.Budget / (1024 * 1024) << " MB\n"
              << "CurrentUsage: " << memoryInfo.CurrentUsage / (1024 * 1024) << " MB\n"
              << "AvailableForReservation: " << memoryInfo.AvailableForReservation / (1024 * 1024) << " MB\n"
              << "CurrentReservation: " << memoryInfo.CurrentReservation / (1024 * 1024) << " MB\n" << std::endl;

    return 0;
}