#include "DWriteEngine.h"

#include <iostream>
#include <stdexcept>
#include <string>

DWriteEngine::DWriteEngine(const Microsoft::WRL::ComPtr<ID2D1RenderTarget>& target)
    : m_renderTarget(target)
{
    HRESULT hr = DWriteCreateFactory(
        DWRITE_FACTORY_TYPE_SHARED,
        __uuidof(IDWriteFactory),
        &m_factory
    );
    if (FAILED(hr))
    {
        std::cerr << "failed to create dwrite factory" << std::endl;
        throw std::runtime_error("failed to create dwrite factory");
    }

    hr = m_factory->CreateTextFormat(
        L"Segoe UI",
        nullptr,
        DWRITE_FONT_WEIGHT_REGULAR,
        DWRITE_FONT_STYLE_NORMAL,
        DWRITE_FONT_STRETCH_NORMAL,
        32.0f,
        L"en-US",
        &m_format
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to create text format" << std::endl;
        throw std::runtime_error("failed to create text format");
    }

    hr = m_renderTarget->CreateSolidColorBrush(
        D2D1::ColorF(D2D1::ColorF::Black),
        &m_brush
    );
    if (FAILED(hr))
    {
        std::cerr << "failed to create solid color brush" << std::endl;
        throw std::runtime_error("failed to create solid color brush");
    }
}

void DWriteEngine::draw() const
{
    std::wstring string = L"Hello, DirectWrite";
    m_renderTarget->DrawTextA(
        string.c_str(),
        string.size(),
        m_format.Get(),
        D2D1::RectF(0, 0, 300, 300),
        m_brush.Get()
    );
}
