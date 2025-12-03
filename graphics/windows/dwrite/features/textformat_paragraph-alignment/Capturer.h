#ifndef CAPTURE_H
#define CAPTURE_H

#include <d3d11.h>
#include <DirectXTex.h>
#include <winrt/windows.graphics.capture.h>
#include <winrt/windows.graphics.directx.direct3d11.h>
#include <wrl/client.h>

class Capturer {
public:
    Capturer(
        const winrt::Windows::Graphics::DirectX::Direct3D11::IDirect3DDevice& device,
        const Microsoft::WRL::ComPtr<ID3D11Device> &d3dDevice,
        const Microsoft::WRL::ComPtr<ID3D11DeviceContext> &d3dContext,
        HWND hwnd
    );
    ~Capturer();

    void StartCapture();
    void Close();

    void Save(int id);
private:
    void OnFrameArrived(
        const winrt::Windows::Graphics::Capture::Direct3D11CaptureFramePool& sender,
        const winrt::Windows::Foundation::IInspectable& args
    );

    winrt::Windows::Graphics::Capture::GraphicsCaptureItem m_graphicsCaptureItem;
    winrt::Windows::Graphics::Capture::Direct3D11CaptureFramePool m_framePool;
    winrt::Windows::Graphics::Capture::GraphicsCaptureSession m_captureSession;
    winrt::Windows::Graphics::SizeInt32 m_lastSize;
    winrt::Windows::Graphics::Capture::Direct3D11CaptureFramePool::FrameArrived_revoker m_frameArrivedRevoker;

    winrt::Windows::Graphics::DirectX::Direct3D11::IDirect3DDevice m_device;
    Microsoft::WRL::ComPtr<ID3D11Device> m_d3dDevice;
    Microsoft::WRL::ComPtr<ID3D11DeviceContext> m_d3dContext;

    winrt::com_ptr<ID3D11Texture2D> m_image;

    bool m_isSaved = false;
    bool m_isSaving = false;
};



#endif //CAPTURE_H
