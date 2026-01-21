#ifndef DWRITE_02_DWRITE_HELLO_DWRITEENGINE_H
#define DWRITE_02_DWRITE_HELLO_DWRITEENGINE_H

#include <wrl/client.h>
#include <d2d1.h>
#include <dwrite.h>

class DWriteEngine
{
public:
    DWriteEngine(const Microsoft::WRL::ComPtr<ID2D1RenderTarget>& target);
    ~DWriteEngine() = default;

    void draw() const;

private:
    Microsoft::WRL::ComPtr<ID2D1RenderTarget> m_renderTarget;

    Microsoft::WRL::ComPtr<IDWriteFactory> m_factory;
    Microsoft::WRL::ComPtr<IDWriteTextFormat> m_format;
    Microsoft::WRL::ComPtr<ID2D1SolidColorBrush> m_brush;
};


#endif //DWRITE_02_DWRITE_HELLO_DWRITEENGINE_H