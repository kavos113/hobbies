#include "Application.h"

#include <string>
#include <format>
#include <iostream>

#define CUSTOM_TIMER_ID 1
#define CUSTOM_TIMER_2 2

Application::Application()
    : m_hwnd(nullptr)
{
    WNDCLASSEX wc = {
        .cbSize = sizeof(WNDCLASSEX),
        .style = CS_HREDRAW | CS_VREDRAW,
        .lpfnWndProc = WindowProc,
        .hInstance = GetModuleHandle(nullptr),
        .hIcon = LoadIcon(nullptr, IDI_APPLICATION),
        .hCursor = LoadCursor(nullptr, IDC_ARROW),
        .hbrBackground = reinterpret_cast<HBRUSH>((COLOR_WINDOW + 1)),
        .lpszClassName = className,
    };

    RegisterClassEx(&wc);
}

Application::~Application()
{
    if (m_hwnd)
    {
        DestroyWindow(m_hwnd);
        m_hwnd = nullptr;
    }

    UnregisterClass(className, GetModuleHandle(nullptr));
}

int Application::createWindow(int x, int y, int width, int height)
{
    HWND hwnd = CreateWindowEx(
        0,
        className,
        L"Hello, World!",
        WS_OVERLAPPEDWINDOW,
        x, y,
        width, height,
        nullptr,
        nullptr,
        GetModuleHandle(nullptr),
        this
    );
    if (hwnd == nullptr)
    {
        return -1;
    }

    ShowWindow(hwnd, SW_SHOW);
    UpdateWindow(hwnd);

    return 0;
}

void CALLBACK TimerProc(HWND hwnd, UINT uMsg, UINT_PTR idEvent, DWORD dwTime)
{
    std::cout << "Timer event: " << idEvent << " at " << dwTime << " ms" << std::endl;
}

void Application::run()
{
    SetTimer(m_hwnd, CUSTOM_TIMER_ID, 1000, nullptr);
    SetTimer(m_hwnd, CUSTOM_TIMER_2, 500, TimerProc);

    MSG msg = {};
    while (GetMessage(&msg, nullptr, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
}

LRESULT Application::WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    Application *app = nullptr;

    if (uMsg == WM_NCCREATE)
    {
        auto *pCreate = reinterpret_cast<CREATESTRUCT *>(lParam);
        app = static_cast<Application *>(pCreate->lpCreateParams);
        SetWindowLongPtr(hwnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(app));

        app->m_hwnd = hwnd;
    }
    else
    {
        app = reinterpret_cast<Application *>(GetWindowLongPtr(hwnd, GWLP_USERDATA));
    }

    if (app)
    {
        return app->handleMessage(uMsg, wParam, lParam);
    }

    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

LRESULT Application::handleMessage(UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;

    case WM_PAINT:
        {
            PAINTSTRUCT ps;
            HDC hdc = BeginPaint(m_hwnd, &ps);

            FillRect(hdc, &ps.rcPaint, static_cast<HBRUSH>(GetStockObject(WHITE_BRUSH)));

            std::wstring text = std::format(L"Hello, World! Count: {}", count);
            DrawText(hdc, text.c_str(), -1, &ps.rcPaint, DT_CENTER | DT_VCENTER | DT_SINGLELINE);

            EndPaint(m_hwnd, &ps);
        }

    case WM_TIMER:
        switch (wParam)
        {
        case CUSTOM_TIMER_ID:
            count++;
            InvalidateRect(m_hwnd, nullptr, FALSE);
            return 0;
        }

    default:
        return DefWindowProc(m_hwnd, uMsg, wParam, lParam);
    }
}
