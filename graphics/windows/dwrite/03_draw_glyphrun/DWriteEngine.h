#ifndef DWRITE_02_DWRITE_HELLO_DWRITEENGINE_H
#define DWRITE_02_DWRITE_HELLO_DWRITEENGINE_H

#include <wrl/client.h>
#include <d2d1.h>
#include <dwrite.h>
#include <string>

class DWriteEngine
{
public:
    DWriteEngine(const Microsoft::WRL::ComPtr<ID2D1RenderTarget>& target);
    ~DWriteEngine() = default;

    void draw() const;

private:
    const std::wstring STRING = L"Hello, DirectWrite!";

    Microsoft::WRL::ComPtr<ID2D1RenderTarget> m_renderTarget;

    Microsoft::WRL::ComPtr<IDWriteFactory> m_factory;
    Microsoft::WRL::ComPtr<IDWriteFontFace> m_face;
    Microsoft::WRL::ComPtr<ID2D1SolidColorBrush> m_brush;
};


#endif //DWRITE_02_DWRITE_HELLO_DWRITEENGINE_H