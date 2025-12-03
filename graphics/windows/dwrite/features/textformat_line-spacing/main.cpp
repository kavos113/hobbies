#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>

#include "App.h"

int WINAPI wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PWSTR pCmdLine, int nCmdShow)
{
    init_apartment(winrt::apartment_type::single_threaded);

    HRESULT hr = CoInitializeEx(nullptr, COINIT_APARTMENTTHREADED);
    if (FAILED(hr))
    {
        return 1;
    }

    App app;
    hr = app.Initialize(L"DWrite Test", WS_OVERLAPPEDWINDOW | WS_VISIBLE, 0, 0, 0, 1366, 768);
    if (FAILED(hr))
    {
        return 1;
    }

    std::thread capture_thread([&app]() {
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
        PostMessage(app.Window(), WM_CUSTOM_CAPTURE, 0, 0);
        while (true)
        {
            int id = 0;
            PostMessage(app.Window(), WM_CUSTOM_ACTION, 0, reinterpret_cast<LPARAM>(&id));
            std::this_thread::sleep_for(std::chrono::milliseconds(1000));
            if (id == -1)
            {
                break;
            }

            PostMessage(app.Window(), WM_CUSTOM_SAVE, id, 0);
        }

        PostMessage(app.Window(), WM_CLOSE, 0, 0);
    });

    MSG msg = {};
    while (GetMessage(&msg, nullptr, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    capture_thread.join();

    return 0;
}
