#include "App.h"

#include <array>
#include <iostream>
#include <windows.graphics.directx.direct3d11.interop.h>

LRESULT App::WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    App* pThis = nullptr;

    if (uMsg == WM_CREATE)
    {
        CREATESTRUCT* pCreate = reinterpret_cast<CREATESTRUCT*>(lParam);
        pThis = static_cast<App*>(pCreate->lpCreateParams);
        SetWindowLongPtr(hwnd, GWLP_USERDATA, reinterpret_cast<LONG_PTR>(pThis));
        pThis->m_hwnd = hwnd;
    }
    else
    {
        pThis = reinterpret_cast<App*>(GetWindowLongPtr(hwnd, GWLP_USERDATA));
    }

    if (pThis)
    {
        return pThis->HandleMessage(uMsg, wParam, lParam);
    }

    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

HRESULT App::Initialize(
    const wchar_t *title,
    DWORD dwStyle,
    DWORD dwExStyle,
    int x,
    int y,
    int nWidth,
    int nHeight,
    HWND hWndParent,
    HMENU hMenu
)
{
    WNDCLASSEX wc = {
        .cbSize = sizeof(WNDCLASSEX),
        .style = CS_HREDRAW | CS_VREDRAW,
        .lpfnWndProc = WndProc,
        .cbClsExtra = 0,
        .cbWndExtra = 0,
        .hInstance = GetModuleHandle(nullptr),
        .hIcon = LoadIcon(nullptr, IDI_APPLICATION),
        .hCursor = LoadCursor(nullptr, IDC_ARROW),
        .hbrBackground = reinterpret_cast<HBRUSH>(COLOR_WINDOW + 1),
        .lpszMenuName = nullptr,
        .lpszClassName = L"App",
        .hIconSm = LoadIcon(nullptr, IDI_APPLICATION)
    };
    ATOM atom = RegisterClassEx(&wc);
    if (!atom)
    {
        return HRESULT_FROM_WIN32(GetLastError());
    }

    m_hwnd = CreateWindowEx(
        dwExStyle,
        wc.lpszClassName,
        title,
        dwStyle,
        x,
        y,
        nWidth,
        nHeight,
        hWndParent,
        hMenu,
        GetModuleHandle(nullptr),
        this
    );
    if (!m_hwnd)
    {
        return HRESULT_FROM_WIN32(GetLastError());
    }

    HRESULT hr = InitD2D();
    if (FAILED(hr))
    {
        return hr;
    }

    winrt::com_ptr<IInspectable> object;
    hr = CreateDirect3D11DeviceFromDXGIDevice(m_dxgiDevice.Get(), object.put());
    if (FAILED(hr))
    {
        std::cerr << "Failed to create Direct3D11 device" << std::endl;
        return hr;
    }
    m_capture = std::make_unique<Capturer>(
        object.as<winrt::Windows::Graphics::DirectX::Direct3D11::IDirect3DDevice>(),
        m_d3dDevice,
        m_d3dContext,
        m_hwnd
    );

    return S_OK;
}

LRESULT App::HandleMessage(UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg)
    {
        case WM_DESTROY:
            PostQuitMessage(0);
            return 0;

        case WM_PAINT:
            Render();
            return 0;

        case WM_CUSTOM_CAPTURE:
            m_capture->StartCapture();
            return 0;

        case WM_CUSTOM_ACTION:
        {
            int* id = reinterpret_cast<int *>(lParam);
            *id = m_dwrite.NextAction();
            InvalidateRect(m_hwnd, nullptr, FALSE);
        }
            return 0;

        case WM_CUSTOM_SAVE:
            m_capture->Save(static_cast<int>(wParam));
            return 0;

        default:
            return DefWindowProc(m_hwnd, uMsg, wParam, lParam);
    }
}

HRESULT App::InitD2D()
{
    std::array feature_levels = {
        D3D_FEATURE_LEVEL_11_1,
        D3D_FEATURE_LEVEL_11_0,
        D3D_FEATURE_LEVEL_10_1,
        D3D_FEATURE_LEVEL_10_0,
        D3D_FEATURE_LEVEL_9_3,
        D3D_FEATURE_LEVEL_9_2,
        D3D_FEATURE_LEVEL_9_1
    };

    HRESULT hr = D3D11CreateDevice(
        nullptr,
        D3D_DRIVER_TYPE_HARDWARE,
        nullptr,
        D3D11_CREATE_DEVICE_BGRA_SUPPORT,
        feature_levels.data(),
        feature_levels.size(),
        D3D11_SDK_VERSION,
        &m_d3dDevice,
        &m_featureLevel,
        &m_d3dContext
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to create D3D11 device" << std::endl;
        return hr;
    }

    hr = m_d3dDevice.As(&m_dxgiDevice);
    if (FAILED(hr))
    {
        std::cerr << "Failed to get DXGI device" << std::endl;
        return hr;
    }

    hr = D2D1CreateFactory(
        D2D1_FACTORY_TYPE_SINGLE_THREADED,
        __uuidof(ID2D1Factory8),
        &m_d2dFactory
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to create D2D1 factory" << std::endl;
        return hr;
    }

    hr = m_d2dFactory->CreateDevice(m_dxgiDevice.Get(), &m_d2dDevice);
    if (FAILED(hr))
    {
        std::cerr << "Failed to create D2D1 device" << std::endl;
        return hr;
    }

    hr = m_d2dDevice->CreateDeviceContext(
        D2D1_DEVICE_CONTEXT_OPTIONS_NONE,
        &m_d2dContext
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to create D2D1 device context" << std::endl;
        return hr;
    }

    DXGI_SWAP_CHAIN_DESC1 swap_chain_desc = {
        .Width = 0,
        .Height = 0,
        .Format = DXGI_FORMAT_B8G8R8A8_UNORM,
        .Stereo = FALSE,
        .SampleDesc = {
            .Count = 1,
            .Quality = 0
        },
        .BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT,
        .BufferCount = 2,
        .Scaling = DXGI_SCALING_NONE,
        .SwapEffect = DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL,
        .AlphaMode = DXGI_ALPHA_MODE_IGNORE,
        .Flags = 0
    };

    Microsoft::WRL::ComPtr<IDXGIAdapter> dxgiAdapter;
    hr = m_dxgiDevice->GetAdapter(&dxgiAdapter);
    if (FAILED(hr))
    {
        std::cerr << "Failed to get DXGI adapter" << std::endl;
        return hr;
    }

    Microsoft::WRL::ComPtr<IDXGIFactory2> dxgiFactory;
    hr = dxgiAdapter->GetParent(__uuidof(IDXGIFactory2), &dxgiFactory);
    if (FAILED(hr))
    {
        std::cerr << "Failed to get DXGI factory" << std::endl;
        return hr;
    }

    hr = dxgiFactory->CreateSwapChainForHwnd(
        m_dxgiDevice.Get(),
        m_hwnd,
        &swap_chain_desc,
        nullptr,
        nullptr,
        &m_swapChain
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to create swap chain" << std::endl;
        return hr;
    }

    Microsoft::WRL::ComPtr<ID3D11Texture2D> back_buffer;
    hr = m_swapChain->GetBuffer(0, __uuidof(ID3D11Texture2D), &back_buffer);
    if (FAILED(hr))
    {
        std::cerr << "Failed to get back buffer" << std::endl;
        return hr;
    }

    D2D1_BITMAP_PROPERTIES1 bitmap_props = {
        .pixelFormat = D2D1::PixelFormat(
            DXGI_FORMAT_B8G8R8A8_UNORM,
            D2D1_ALPHA_MODE_PREMULTIPLIED
        ),
        .dpiX = 96.0f,
        .dpiY = 96.0f,
        .bitmapOptions = D2D1_BITMAP_OPTIONS_TARGET | D2D1_BITMAP_OPTIONS_CANNOT_DRAW,
    };

    Microsoft::WRL::ComPtr<IDXGISurface> dxgiSurface;
    hr = m_swapChain->GetBuffer(0, __uuidof(IDXGISurface), &dxgiSurface);
    if (FAILED(hr))
    {
        std::cerr << "Failed to get DXGI surface" << std::endl;
        return hr;
    }

    hr = m_d2dContext->CreateBitmapFromDxgiSurface(
        dxgiSurface.Get(),
        &bitmap_props,
        &m_d2dBitmap
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to create D2D1 target bitmap" << std::endl;
        return hr;
    }

    m_d2dContext->SetTarget(m_d2dBitmap.Get());

    hr = m_d2dContext->CreateSolidColorBrush(
        D2D1::ColorF(D2D1::ColorF::Black),
        &m_brush
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to create solid color brush" << std::endl;
        return hr;
    }

    m_textRect = {
        .left = 10.0f,
        .top = 10.0f,
        .right = 500.0f,
        .bottom = 500.0f
    };
    m_dwrite.Init(m_textRect, m_d2dContext);

    return S_OK;
}

HRESULT App::Render()
{
    m_d2dContext->BeginDraw();

    m_d2dContext->Clear(D2D1::ColorF(D2D1::ColorF::White));

    m_dwrite.Render();
    m_d2dContext->DrawRectangle(m_textRect, m_brush.Get());

    HRESULT hr = m_d2dContext->EndDraw();
    if (FAILED(hr))
    {
        std::cerr << "Failed to end draw" << std::endl;
        return hr;
    }

    hr = m_swapChain->Present(1, 0);
    if (FAILED(hr))
    {
        std::cerr << "Failed to present" << std::endl;
        return hr;
    }

    return S_OK;
}
