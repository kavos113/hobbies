#ifndef APP_H
#define APP_H

#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <wrl/client.h>
#include <d2d1_3.h>
#include <dxgi1_6.h>
#include <d3d11.h>

#include "DWrite.h"
#include "Capturer.h"

#define WM_CUSTOM_CAPTURE (WM_USER + 1)
#define WM_CUSTOM_ACTION (WM_USER + 2)
#define WM_CUSTOM_SAVE (WM_USER + 3)

class App {
public:
    App() : m_hwnd(nullptr), m_featureLevel(D3D_FEATURE_LEVEL_11_1) {}

    static LRESULT CALLBACK WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);

    HRESULT Initialize(
        const wchar_t* title,
        DWORD dwStyle,
        DWORD dwExStyle = 0,
        int x = CW_USEDEFAULT,
        int y = CW_USEDEFAULT,
        int nWidth = CW_USEDEFAULT,
        int nHeight = CW_USEDEFAULT,
        HWND hWndParent = nullptr,
        HMENU hMenu = nullptr
    );

    HWND Window() const { return m_hwnd; }

private:
    LRESULT HandleMessage(UINT uMsg, WPARAM wParam, LPARAM lParam);

    HRESULT InitD2D();
    HRESULT Render();

    DWrite m_dwrite;
    D2D1_RECT_F m_textRect;

    std::unique_ptr<Capturer> m_capture;

    HWND m_hwnd;

    Microsoft::WRL::ComPtr<ID2D1Factory8> m_d2dFactory;
    Microsoft::WRL::ComPtr<ID2D1Device7> m_d2dDevice;
    Microsoft::WRL::ComPtr<ID2D1DeviceContext7> m_d2dContext;
    Microsoft::WRL::ComPtr<IDXGISwapChain1> m_swapChain;
    Microsoft::WRL::ComPtr<ID2D1Bitmap1> m_d2dBitmap;
    Microsoft::WRL::ComPtr<ID2D1SolidColorBrush> m_brush;

    Microsoft::WRL::ComPtr<ID3D11Device> m_d3dDevice;
    Microsoft::WRL::ComPtr<ID3D11DeviceContext> m_d3dContext;
    Microsoft::WRL::ComPtr<IDXGIDevice4> m_dxgiDevice;

    D3D_FEATURE_LEVEL m_featureLevel;
};



#endif //APP_H
