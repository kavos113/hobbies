#ifndef CAPTURE_H
#define CAPTURE_H

#include <d3d11.h>
#include <winrt/windows.graphics.capture.h>
#include <winrt/windows.graphics.directx.direct3d11.h>

class Capture {
public:
    Capture(ID3D11Device* device);
    ~Capture();

    int GetHeight() const
    {
        return m_lastSize.Height;
    }

    int GetWidth() const
    {
        return m_lastSize.Width;
    }

    void StartCapture();
private:
    void OnFrameArrived(const winrt::Windows::Graphics::Capture::Direct3D11CaptureFramePool& sender, const winrt::Windows::Foundation::IInspectable& args);

    winrt::Windows::Graphics::DirectX::Direct3D11::IDirect3DDevice m_device{ nullptr };
    winrt::Windows::Graphics::Capture::GraphicsCaptureItem m_graphicsCaptureItem{ nullptr };
    winrt::Windows::Graphics::Capture::GraphicsCaptureSession m_graphicsCaptureSession{ nullptr };
    winrt::Windows::Graphics::Capture::Direct3D11CaptureFramePool m_framePool{ nullptr };
    winrt::Windows::Graphics::Capture::Direct3D11CaptureFramePool::FrameArrived_revoker m_frameArrivedRevoker;
    winrt::Windows::Graphics::SizeInt32 m_lastSize{ 0, 0 };
};



#endif //CAPTURE_H
