#include "DWriteEngine.h"

#include <iostream>
#include <stdexcept>
#include <vector>

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

    hr = m_renderTarget->CreateSolidColorBrush(
        D2D1::ColorF(D2D1::ColorF::Black),
        &m_brush
    );
    if (FAILED(hr))
    {
        std::cerr << "failed to create solid color brush" << std::endl;
        throw std::runtime_error("failed to create solid color brush");
    }

    Microsoft::WRL::ComPtr<IDWriteFontCollection> fontCollection;
    hr = m_factory->GetSystemFontCollection(&fontCollection);
    if (FAILED(hr))
    {
        std::cerr << "failed to get system font collection" << std::endl;
        throw std::runtime_error("failed to get system font collection");
    }

    UINT32 index;
    BOOL exist;
    hr = fontCollection->FindFamilyName(L"Arial", &index, &exist);
    if (FAILED(hr))
    {
        std::cerr << "failed to find font family" << std::endl;
        throw std::runtime_error("failed to find font family");
    }

    Microsoft::WRL::ComPtr<IDWriteFontFamily> fontFamily;
    hr = fontCollection->GetFontFamily(index, &fontFamily);
    if (FAILED(hr))
    {
        std::cerr << "failed get font family" << std::endl;
        throw std::runtime_error("failed get font family");
    }

    Microsoft::WRL::ComPtr<IDWriteFont> font;
    hr = fontFamily->GetFirstMatchingFont(
        DWRITE_FONT_WEIGHT_NORMAL,
        DWRITE_FONT_STRETCH_NORMAL,
        DWRITE_FONT_STYLE_NORMAL,
        &font
    );
    if (FAILED(hr))
    {
        std::cerr << "failed to get font" << std::endl;
        throw std::runtime_error("failed to get font");
    }

    hr = font->CreateFontFace(&m_face);
    if (FAILED(hr))
    {
        std::cerr << "failed to create font face" << std::endl;
        throw std::runtime_error("failed to create font face");
    }
}

void DWriteEngine::draw() const
{
    UINT32 textLength = static_cast<UINT32>(STRING.size());
    std::vector<UINT16> glyphIndices(textLength);

    // latin only
    std::vector<UINT32> codePoints(textLength);
    for (int i = 0; i < textLength; ++i)
    {
        codePoints[i] = STRING[i];
    }

    HRESULT hr = m_face->GetGlyphIndicesA(codePoints.data(), codePoints.size(), glyphIndices.data());
    if (FAILED(hr))
    {
        std::cerr << "failed to get glyph indices" << std::endl;
        return;
    }

    std::vector<DWRITE_GLYPH_METRICS> glyphMetrics(textLength);
    hr = m_face->GetDesignGlyphMetrics(glyphIndices.data(), textLength, glyphMetrics.data());
    if (FAILED(hr))
    {
        std::cerr << "failed to get glyph metrics" << std::endl;
        return;
    }

    DWRITE_FONT_METRICS metrics{};
    m_face->GetMetrics(&metrics);

    float fontSize = 36.0f;
    float scale = fontSize / (float)metrics.designUnitsPerEm;

    std::vector<float> glyphAdvances(textLength);
    for (int i = 0; i < textLength; ++i)
    {
        glyphAdvances[i] = glyphMetrics[i].advanceWidth * scale;
    }

    for (int i = 0; i < textLength; ++i)
    {
        std::wcout << "Glyph " << STRING[i] << " advanceWidth: " << glyphMetrics[i].advanceWidth <<
            ", advanceHeight: " << glyphMetrics[i].advanceHeight << ", leftSideBearing: " << glyphMetrics[i].leftSideBearing <<
            ", topSideBearing: " << glyphMetrics[i].topSideBearing << std::endl;
    }

    DWRITE_GLYPH_RUN glyphRun = {
        .fontFace = m_face.Get(),
        .fontEmSize = fontSize,
        .glyphCount = textLength,
        .glyphIndices = glyphIndices.data(),
        .glyphAdvances = glyphAdvances.data(),
        .glyphOffsets = nullptr,
        .isSideways = FALSE,
        .bidiLevel = 0
    };

    m_renderTarget->DrawGlyphRun(
        D2D1::Point2F(100, 100),
        &glyphRun,
        m_brush.Get(),
        DWRITE_MEASURING_MODE_NATURAL
    );
}
