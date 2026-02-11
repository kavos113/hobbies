#define WIN32_LEAN_AND_MEAN 1

#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <pdh.h>

#include <iostream>
#include <vector>

class Monitor
{
public:
    Monitor(const wchar_t *query = L"\\GPU Adapter Memory(*)\\Dedicated Usage")
    {
        PDH_STATUS status = PdhOpenQuery(nullptr, 0, &m_query);
        if (status != ERROR_SUCCESS || m_query == nullptr)
        {
            std::cerr << "failed to open query" << std::endl;
            exit(1);
        }

        status = PdhAddCounter(m_query, query, 0, &m_counter);
        if (status != ERROR_SUCCESS || m_counter == nullptr)
        {
            std::cerr << "failed to add counter: " << status << std::endl;
            exit(1);
        }
    }

    ~Monitor()
    {
        if (m_query)
        {
            PdhCloseQuery(m_query);
        }
    }

    void ShowValue() const
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
        if (bufferSize == 0)
        {
            return;
        }

        std::vector<std::byte> buf(bufferSize);
        PPDH_FMT_COUNTERVALUE_ITEM items = reinterpret_cast<PPDH_FMT_COUNTERVALUE_ITEM_W>(buf.data());
        PdhGetFormattedCounterArray(m_counter, PDH_FMT_LARGE, &bufferSize, &count, items);

        for (DWORD i = 0; i < count; i++)
        {
            std::wcout << items[i].szName << ": "
                << items[i].FmtValue.doubleValue
                << "  " << items[i].FmtValue.largeValue
                << std::endl;
        }
    }
private:
    HQUERY m_query = nullptr;
    HCOUNTER m_counter = nullptr;
};

int main()
{
    WCHAR pathBuffer[PDH_MAX_COUNTER_PATH] = {};

    PDH_BROWSE_DLG_CONFIG browseDlgConfig = {};
    browseDlgConfig.bIncludeInstanceIndex = FALSE;
    browseDlgConfig.bSingleCounterPerAdd = TRUE;
    browseDlgConfig.bSingleCounterPerDialog = TRUE;
    browseDlgConfig.bLocalCountersOnly = FALSE;
    browseDlgConfig.bWildCardInstances = TRUE;
    browseDlgConfig.bHideDetailBox = TRUE;
    browseDlgConfig.bInitializePath = FALSE;
    browseDlgConfig.bDisableMachineSelection = FALSE;
    browseDlgConfig.bIncludeCostlyObjects = FALSE;
    browseDlgConfig.bShowObjectBrowser = FALSE;

    browseDlgConfig.hWndOwner = nullptr;
    browseDlgConfig.szDataSource = nullptr;
    browseDlgConfig.szReturnPathBuffer = pathBuffer;
    browseDlgConfig.cchReturnPathLength = PDH_MAX_COUNTER_PATH;
    browseDlgConfig.pCallBack = nullptr;
    browseDlgConfig.dwCallBackArg = 0;
    browseDlgConfig.CallBackStatus = ERROR_SUCCESS;
    browseDlgConfig.dwDefaultDetailLevel = PERF_DETAIL_WIZARD;
    browseDlgConfig.szDialogBoxCaption = const_cast<LPWSTR>(L"Select counter");

    PDH_STATUS status = PdhBrowseCounters(&browseDlgConfig);
    if (status != ERROR_SUCCESS)
    {
        std::cerr << "failed to browse counters: " << status << std::endl;
        return -1;
    }
    if (wcslen(pathBuffer) == 0)
    {
        std::cerr << "no counter selected" << std::endl;
        return -1;
    }

    std::wcout << L"Selected counter path: " << pathBuffer << std::endl;

    Monitor monitor(pathBuffer);
    while (true)
    {
        monitor.ShowValue();
        Sleep(1000);
    }

    return 0;
}