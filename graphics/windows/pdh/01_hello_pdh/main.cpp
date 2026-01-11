#define WIN32_LEAN_AND_MEAN 1

#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <pdh.h>

#include <iostream>
#include <vector>

class GPUMonitor
{
public:
    GPUMonitor()
    {
        PDH_STATUS status = PdhOpenQuery(nullptr, 0, &m_query);
        if (status != ERROR_SUCCESS || m_query == nullptr)
        {
            std::cerr << "failed to open query" << std::endl;
            exit(1);
        }

        status = PdhAddCounter(m_query, GPU_MEMORY_QUERY, 0, &m_counter);
        if (status != ERROR_SUCCESS || m_counter == nullptr)
        {
            std::cerr << "failed to add counter: " << status << std::endl;
            exit(1);
        }
    }

    ~GPUMonitor()
    {
        if (m_query)
        {
            PdhCloseQuery(m_query);
        }
    }

    void ShowMemoryUsage() const
    {
        if (!m_query || !m_counter)
        {
            return;
        }

        PDH_STATUS status = PdhCollectQueryData(m_query);
        if (status != ERROR_SUCCESS)
        {
            std::cerr << "failed to collect query data: " << status << std::endl;
            exit(1);
        }

        DWORD bufferSize = 0;
        DWORD count = 0;
        PdhGetFormattedCounterArray(m_counter, PDH_FMT_LARGE, &bufferSize, &count, nullptr);

        std::cout << "count: " << count << std::endl;

        if (bufferSize == 0)
        {
            return;
        }

        std::vector<std::byte> buf(bufferSize);
        PPDH_FMT_COUNTERVALUE_ITEM items = reinterpret_cast<PPDH_FMT_COUNTERVALUE_ITEM_W>(buf.data());
        PdhGetFormattedCounterArray(m_counter, PDH_FMT_LARGE, &bufferSize, &count, items);

        for (DWORD i = 0; i < count; i++)
        {
            std::wcout << items[i].szName << ": " << items[i].FmtValue.largeValue << std::endl;;
        }
    }
private:
    HQUERY m_query = nullptr;
    HCOUNTER m_counter = nullptr;

    const wchar_t *GPU_MEMORY_QUERY = L"\\GPU Adapter Memory(*)\\Dedicated Usage";
};

int main()
{
    GPUMonitor monitor;
    monitor.ShowMemoryUsage();

    return 0;
}