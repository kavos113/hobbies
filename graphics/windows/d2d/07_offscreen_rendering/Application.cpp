#include "Application.h"

#include <array>
#include <d3d11.h>
#include <wincodec.h>

void Application::initD2D()
{
    HRESULT hr = CoInitializeEx(nullptr, COINIT_MULTITHREADED);
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to initialize COM", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    hr = D2D1CreateFactory(
        D2D1_FACTORY_TYPE_SINGLE_THREADED,
        m_d2dFactory.GetAddressOf()
    );
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to create D2D factory", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    UINT creationFlags = D3D11_CREATE_DEVICE_BGRA_SUPPORT;

    std::array featureLevels = {
        D3D_FEATURE_LEVEL_11_1,
        D3D_FEATURE_LEVEL_11_0,
        D3D_FEATURE_LEVEL_10_1,
        D3D_FEATURE_LEVEL_10_0,
        D3D_FEATURE_LEVEL_9_3,
        D3D_FEATURE_LEVEL_9_2,
        D3D_FEATURE_LEVEL_9_1
    };

    Microsoft::WRL::ComPtr<ID3D11Device> d3dDevice;
    Microsoft::WRL::ComPtr<ID3D11DeviceContext> d3dContext;
    hr = D3D11CreateDevice(
        nullptr, // Use default adapter
        D3D_DRIVER_TYPE_HARDWARE,
        nullptr, // No software device
        creationFlags,
        featureLevels.data(), featureLevels.size(),
        D3D11_SDK_VERSION,
        &d3dDevice,
        nullptr, // Feature level not needed
        &d3dContext
    );
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to create D3D11 device", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    Microsoft::WRL::ComPtr<IDXGIDevice> dxgiDevice;
    hr = d3dDevice.As(&dxgiDevice);
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to get IDXGIDevice", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    hr = m_d2dFactory->CreateDevice(dxgiDevice.Get(), &m_d2dDevice);
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to create D2D device", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    hr = m_d2dDevice->CreateDeviceContext(
        D2D1_DEVICE_CONTEXT_OPTIONS_NONE,
        &m_d2dContext
    );
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to create D2D device context", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    createSurfaceBitmap();

    createResources();
}

void Application::createSurfaceBitmap()
{
    D2D1_BITMAP_PROPERTIES1 bitmapProperties = {
        .pixelFormat = {
            .format = DXGI_FORMAT_B8G8R8A8_UNORM,
            .alphaMode = D2D1_ALPHA_MODE_PREMULTIPLIED
        },
        .dpiX = 96.0f,
        .dpiY = 96.0f,
        .bitmapOptions = D2D1_BITMAP_OPTIONS_TARGET | D2D1_BITMAP_OPTIONS_CANNOT_DRAW
    };

    HRESULT hr = m_d2dContext->CreateBitmap(
        D2D1::SizeU(m_windowRect.right - m_windowRect.left, m_windowRect.bottom - m_windowRect.top),
        nullptr,
        0,
        &bitmapProperties,
        &m_bitmap
    );
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to create D2D bitmap", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    m_d2dContext->SetTarget(m_bitmap.Get());
}

void Application::createResources()
{
    HRESULT hr = m_d2dContext->CreateSolidColorBrush(
        D2D1::ColorF(D2D1::ColorF::Red),
        &m_brush
    );
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to create D2D solid color brush", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    hr = m_d2dContext->CreateSolidColorBrush(
        D2D1::ColorF(D2D1::ColorF::Green),
        &m_greenBrush
    );
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to create D2D green brush", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    D2D1_STROKE_STYLE_PROPERTIES strokeStyleProperties = {
        .startCap = D2D1_CAP_STYLE_ROUND,
        .endCap = D2D1_CAP_STYLE_SQUARE,
        .dashCap = D2D1_CAP_STYLE_TRIANGLE,
        .lineJoin = D2D1_LINE_JOIN_MITER,
        .miterLimit = 10.0f,
        .dashStyle = D2D1_DASH_STYLE_CUSTOM,
        .dashOffset = 0.0f,
    };
    std::array dashes = { 5.0f, 2.0f };
    hr = m_d2dFactory->CreateStrokeStyle(
        &strokeStyleProperties,
        dashes.data(),
        dashes.size(),
        &m_strokeStyle
    );
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to create D2D stroke style", L"Error", MB_OK | MB_ICONERROR);
        return;
    }
}

void Application::onPaint()
{
    m_d2dContext->BeginDraw();
    m_d2dContext->Clear(D2D1::ColorF(D2D1::ColorF::SkyBlue));

    m_d2dContext->FillRectangle(D2D1::RectF(100, 100, 300, 300), m_brush.Get());
    m_d2dContext->FillEllipse(D2D1::Ellipse(D2D1::Point2F(400, 200), 50, 50), m_brush.Get());
    m_d2dContext->FillRoundedRectangle(
        D2D1::RoundedRect(D2D1::RectF(500, 100, 700, 300), 40, 20),
        m_brush.Get()
    );
    m_d2dContext->DrawLine(
        D2D1::Point2F(100, 400),
        D2D1::Point2F(300, 600),
        m_brush.Get(),
        15.0f,
        m_strokeStyle.Get()
    );
    m_d2dContext->DrawRectangle(
        D2D1::RectF(400, 400, 600, 600),
        m_greenBrush.Get(),
        10.0f,
        m_strokeStyle.Get()
    );
    m_d2dContext->DrawEllipse(
        D2D1::Ellipse(D2D1::Point2F(700, 500), 50, 50),
        m_greenBrush.Get(),
        3.0f,
        m_strokeStyle.Get()
    );

    if (FAILED(m_d2dContext->EndDraw()))
    {
        m_d2dContext.Reset();
        m_brush.Reset();
    }
}

void Application::saveImage(const wchar_t* filename) const
{
    Microsoft::WRL::ComPtr<IWICImagingFactory2> wicFactory;
    HRESULT hr = CoCreateInstance(
        CLSID_WICImagingFactory2,
        nullptr,
        CLSCTX_INPROC_SERVER,
        IID_PPV_ARGS(&wicFactory)
    );
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to create WIC Imaging Factory", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    Microsoft::WRL::ComPtr<IWICBitmapEncoder> encoder;
    hr = wicFactory->CreateEncoder(
        GUID_ContainerFormatPng,
        nullptr,
        &encoder
    );
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to create WIC Bitmap Encoder", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    Microsoft::WRL::ComPtr<IWICStream> stream;
    hr = wicFactory->CreateStream(&stream);
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to create WIC Stream", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    hr = stream->InitializeFromFilename(filename, GENERIC_WRITE);
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to initialize WIC Stream from filename", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    hr = encoder->Initialize(stream.Get(), WICBitmapEncoderNoCache);
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to initialize WIC Bitmap Encoder", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    Microsoft::WRL::ComPtr<IWICBitmapFrameEncode> frameEncode;
    Microsoft::WRL::ComPtr<IPropertyBag2> propertyBag;
    hr = encoder->CreateNewFrame(&frameEncode, &propertyBag);
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to create new frame for WIC Bitmap Encoder", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    hr = frameEncode->Initialize(propertyBag.Get());
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to initialize frame for WIC Bitmap Encoder", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    Microsoft::WRL::ComPtr<IWICImageEncoder> wicImageEncoder;
    hr = wicFactory->CreateImageEncoder(m_d2dDevice.Get(), &wicImageEncoder);
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to create WIC Image Encoder", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    hr = wicImageEncoder->WriteFrame(m_bitmap.Get(), frameEncode.Get(), nullptr);
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to write frame using WIC Image Encoder", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    hr = frameEncode->Commit();
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to commit frame for WIC Bitmap Encoder", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    hr = encoder->Commit();
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to commit WIC Bitmap Encoder", L"Error", MB_OK | MB_ICONERROR);
        return;
    }

    hr = stream->Commit(STGC_DEFAULT);
    if (FAILED(hr))
    {
        MessageBox(nullptr, L"Failed to commit WIC Stream", L"Error", MB_OK | MB_ICONERROR);
        return;
    }
}

Application::Application(int width, int height)
{
    m_windowRect = { 0, 0, width, height };
}

Application::~Application() = default;

void Application::run(const wchar_t* outputFilename)
{
    onPaint();
    saveImage(outputFilename);
}
