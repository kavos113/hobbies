#include "Capture.h"

#include <dxgi.h>
#include <iostream>
#include <inspectable.h>
#include <winrt/base.h>
#include <windows.graphics.directx.direct3d11.interop.h>
#include <windows.graphics.capture.interop.h>

Capture::Capture(ID3D11Device *device)
{
    winrt::com_ptr<IDXGIDevice> dxgiDevice;
    winrt::hresult hr = device->QueryInterface<IDXGIDevice>(dxgiDevice.put());
    if (FAILED(hr))
    {
        std::cerr << "Failed to get IDXGIDevice" << std::endl;
        return;
    }

    winrt::com_ptr<IInspectable> object;
    hr = CreateDirect3D11DeviceFromDXGIDevice(dxgiDevice.get(), object.put());
    if (FAILED(hr))
    {
        std::cerr << "Failed to create Direct3D11Device" << std::endl;
        return;
    }

    m_device = object.as<winrt::Windows::Graphics::DirectX::Direct3D11::IDirect3DDevice>();

    winrt::Windows::Foundation::IActivationFactory factory = winrt::get_activation_factory<winrt::Windows::Graphics::Capture::GraphicsCaptureItem>();
    winrt::impl::com_ref<IGraphicsCaptureItemInterop> interop = factory.as<IGraphicsCaptureItemInterop>();
    HWND hwnd = WindowFromPoint(POINT{100, 100});
    hr = interop->CreateForWindow(
        hwnd,
        winrt::guid_of<winrt::Windows::Graphics::Capture::IGraphicsCaptureItem>(),
        winrt::put_abi(m_graphicsCaptureItem)
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to create GraphicsCaptureItem" << std::endl;
        return;
    }

    m_lastSize = m_graphicsCaptureItem.Size();

    m_framePool = winrt::Windows::Graphics::Capture::Direct3D11CaptureFramePool::Create(
        m_device,
        winrt::Windows::Graphics::DirectX::DirectXPixelFormat::B8G8R8A8UIntNormalized,
        2,
        m_lastSize
    );

    m_frameArrivedRevoker = m_framePool.FrameArrived(
        winrt::auto_revoke,
        {this, &Capture::OnFrameArrived}
    );

    m_graphicsCaptureSession = m_framePool.CreateCaptureSession(m_graphicsCaptureItem);
}

Capture::~Capture()
{
    m_frameArrivedRevoker.revoke();

    m_graphicsCaptureSession.Close();
    m_graphicsCaptureSession = nullptr;
    m_framePool.Close();
    m_framePool = nullptr;

    m_device = nullptr;
    m_graphicsCaptureItem = nullptr;
}

void Capture::StartCapture()
{
    if (m_graphicsCaptureSession != nullptr)
    {
        m_graphicsCaptureSession.StartCapture();
    }
}

void Capture::OnFrameArrived(
    const winrt::Windows::Graphics::Capture::Direct3D11CaptureFramePool &sender,
    const winrt::Windows::Foundation::IInspectable &args
)
{
    winrt::Windows::Graphics::Capture::Direct3D11CaptureFrame frame = sender.TryGetNextFrame();
    if (frame == nullptr)
    {
        return;
    }

    winrt::Windows::Graphics::DirectX::Direct3D11::IDirect3DSurface surface = frame.Surface();
    if (surface == nullptr)
    {
        return;
    }

    auto access = surface.as<Windows::Graphics::DirectX::Direct3D11::IDirect3DDxgiInterfaceAccess>();
    winrt::com_ptr<ID3D11Texture2D> texture;
    winrt::hresult hr = access->GetInterface(winrt::guid_of<ID3D11Texture2D>(), texture.put_void());
    if (FAILED(hr))
    {
        return;
    }

    winrt::Windows::Graphics::SizeInt32 size = m_graphicsCaptureItem.Size();



    if ((size.Height != m_lastSize.Height) || (size.Width != m_lastSize.Width))
    {
        m_lastSize = size;
        m_framePool.Recreate(m_device, winrt::Windows::Graphics::DirectX::DirectXPixelFormat::B8G8R8A8UIntNormalized, 2, m_lastSize);
    }
}
