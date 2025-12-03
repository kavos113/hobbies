#include "Capturer.h"

#include <windows.graphics.directx.direct3d11.interop.h>
#include <DirectXTex.h>
#include <iostream>
#include <windows.graphics.capture.interop.h>
#include <winrt/windows.foundation.h>

Capturer::Capturer(
    const winrt::Windows::Graphics::DirectX::Direct3D11::IDirect3DDevice &device,
    const Microsoft::WRL::ComPtr<ID3D11Device> &d3dDevice,
    const Microsoft::WRL::ComPtr<ID3D11DeviceContext> &d3dContext,
    HWND hwnd
)
    : m_graphicsCaptureItem(nullptr)
    , m_framePool(nullptr)
    , m_captureSession(nullptr)
    , m_lastSize({0, 0})
    , m_device(device)
    , m_d3dDevice(d3dDevice)
    , m_d3dContext(d3dContext)
{
    auto activation_factory = winrt::get_activation_factory<winrt::Windows::Graphics::Capture::GraphicsCaptureItem>();
    auto interop_factory = activation_factory.as<IGraphicsCaptureItemInterop>();
    interop_factory->CreateForWindow(
        hwnd,
        winrt::guid_of<winrt::Windows::Graphics::Capture::GraphicsCaptureItem>(),
        winrt::put_abi(m_graphicsCaptureItem)
    );

    auto size = m_graphicsCaptureItem.Size();

    m_framePool = winrt::Windows::Graphics::Capture::Direct3D11CaptureFramePool::Create(
        m_device,
        winrt::Windows::Graphics::DirectX::DirectXPixelFormat::B8G8R8A8UIntNormalized,
        2,
        size
    );
    m_captureSession = m_framePool.CreateCaptureSession(m_graphicsCaptureItem);
    m_lastSize = size;
    m_frameArrivedRevoker = m_framePool.FrameArrived(winrt::auto_revoke, {this, &Capturer::OnFrameArrived});
}

Capturer::~Capturer()
{
    Close();
}

void Capturer::StartCapture()
{
    m_captureSession.StartCapture();
}

void Capturer::Close()
{
    m_frameArrivedRevoker.revoke();

    m_framePool.Close();
    m_framePool = nullptr;

    m_captureSession.Close();
    m_captureSession = nullptr;

    m_lastSize = {0, 0};
    m_isSaved = false;
}


void Capturer::OnFrameArrived(
    const winrt::Windows::Graphics::Capture::Direct3D11CaptureFramePool &sender,
    const winrt::Windows::Foundation::IInspectable &args
)
{
    auto frame = sender.TryGetNextFrame();
    auto size = frame.ContentSize();

    if (m_isSaving)
    {
        return;
    }

    winrt::Windows::Foundation::IInspectable object = frame.Surface();
    auto access = object.as<Windows::Graphics::DirectX::Direct3D11::IDirect3DDxgiInterfaceAccess>();
    winrt::hresult hr = access->GetInterface(winrt::guid_of<ID3D11Texture2D>(), m_image.put_void());
    if (FAILED(hr))
    {
        std::cerr << "Failed to get texture" << std::endl;
        return;
    }

    m_lastSize = size;
}

void Capturer::Save(int id)
{
    m_isSaving = true;

    DirectX::ScratchImage scratchImage;

    HRESULT hr = CaptureTexture(m_d3dDevice.Get(), m_d3dContext.Get(), m_image.get(), scratchImage);
    if (FAILED(hr))
    {
        std::cerr << "Failed to capture texture" << std::endl;
        return;
    }

    std::wstring file_path = std::format(L"capture{}.png", id);
    hr = SaveToWICFile(
        *scratchImage.GetImage(0, 0, 0),
        DirectX::WIC_FLAGS_NONE,
        GetWICCodec(DirectX::WIC_CODEC_PNG),
        file_path.c_str()
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to save image" << std::endl;
        return;
    }

    m_isSaving = false;
}
