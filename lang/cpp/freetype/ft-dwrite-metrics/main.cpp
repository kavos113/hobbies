#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_TRUETYPE_TABLES_H

#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <dwrite.h>
#include <wrl/client.h>

#include <print>
#include <string>
#include <vector>
#include <iostream>

char character = 'A';

void freetype()
{
    FT_Library library;

    FT_Error error = FT_Init_FreeType(&library);
    if (error)
    {
        std::println("Error initializing FreeType: {}", error);
        return;
    }

    FT_Face face;
    error = FT_New_Face(
        library,
        "C:/Windows/Fonts/arial.ttf",
        0,
        &face
    );
    if (error == FT_Err_Unknown_File_Format)
    {
        std::println("The font file could be opened and read, but it appears that its format is unsupported.");
        FT_Done_FreeType(library);
        return;
    }
    if (error)
    {
        std::println("Error loading font: {}", error);
        FT_Done_FreeType(library);
        return;
    }

    error = FT_Set_Pixel_Sizes(face, 0, face->units_per_EM);
    if (error)
    {
        std::println("Error setting character size: {}", error);
        FT_Done_Face(face);
        FT_Done_FreeType(library);
        return;
    }

    auto ascender = static_cast<float>(face->size->metrics.ascender) / 64.0f;
    auto descender = static_cast<float>(face->size->metrics.descender) / 64.0f;
    auto height = static_cast<float>(face->size->metrics.height) / 64.0f;
    auto maxAdvance = static_cast<float>(face->size->metrics.max_advance) / 64.0f;

    std::println("FreeType: \n  Ascender: {}\n  Descender: {}\n  Height: {}\n  Max Advance: {}",
                 ascender, descender, height, maxAdvance);

    error = FT_Load_Char(face, character, FT_LOAD_NO_BITMAP | FT_LOAD_NO_SCALE);
    if (error)
    {
        std::println("Error loading character '{}': {}", character, error);
    }
    else
    {
        auto *glyph = face->glyph;
        std::println("Glyph for '{}': \n  Advance X: {}\n  Bearing X: {}\n  Bearing Y: {}\n  Width: {}\n  Height: {}",
                        character,
                        static_cast<float>(glyph->metrics.horiAdvance),
                        static_cast<float>(glyph->metrics.horiBearingX),
                        static_cast<float>(glyph->metrics.horiBearingY),
                        static_cast<float>(glyph->metrics.width),
                        static_cast<float>(glyph->metrics.height));
    }

    FT_Done_Face(face);
    FT_Done_FreeType(library);
}

void freetype_dwritelike()
{
    FT_Library library;

    FT_Error error = FT_Init_FreeType(&library);
    if (error)
    {
        std::println("Error initializing FreeType: {}", error);
        return;
    }

    FT_Face face;
    error = FT_New_Face(
        library,
        "C:/Windows/Fonts/arial.ttf",
        0,
        &face
    );
    if (error == FT_Err_Unknown_File_Format)
    {
        std::println("The font file could be opened and read, but it appears that its format is unsupported.");
        FT_Done_FreeType(library);
        return;
    }
    if (error)
    {
        std::println("Error loading font: {}", error);
        FT_Done_FreeType(library);
        return;
    }

    DWRITE_FONT_METRICS metrics = {};

    metrics.designUnitsPerEm = face->units_per_EM;

    auto *os2 = reinterpret_cast<TT_OS2 *>(FT_Get_Sfnt_Table(face, ft_sfnt_os2));
    auto *hhea = reinterpret_cast<TT_HoriHeader *>(FT_Get_Sfnt_Table(face, ft_sfnt_hhea));

    if (os2)
    {
        std::println("OS/2 Table found.");

        metrics.ascent = os2->usWinAscent;
        metrics.descent = os2->usWinDescent;

        short lineGap = os2->sTypoLineGap;
        metrics.lineGap = static_cast<signed short>(metrics.designUnitsPerEm + lineGap) - (metrics.ascent + metrics.descent);
    }
    else if (hhea)
    {
        std::println("hhea Table found.");

        metrics.ascent = hhea->Ascender;
        metrics.descent = std::abs(hhea->Descender);
        metrics.lineGap = hhea->Line_Gap;
    }
    else
    {
        std::println("Neither OS/2 nor hhea Table found. Falling back to FreeType metrics.");

        metrics.ascent = face->ascender;
        metrics.descent = std::abs(face->descender);
        metrics.lineGap = face->height - (face->ascender - face->descender);
    }

    if (os2 && os2->version >= 2)
    {
        metrics.capHeight = os2->sCapHeight;
        metrics.xHeight = os2->sxHeight;
    }
    else
    {
        FT_Set_Pixel_Sizes(face, 0, metrics.designUnitsPerEm);

        uint32_t index = FT_Get_Char_Index(face, 'H');
        if (index)
        {
            FT_Load_Glyph(face, index, FT_LOAD_NO_SCALE);
            metrics.capHeight = face->glyph->metrics.horiBearingY;
        }
        index = FT_Get_Char_Index(face, 'x');
        if (index)
        {
            FT_Load_Glyph(face, index, FT_LOAD_NO_SCALE);
            metrics.xHeight = face->glyph->metrics.horiBearingY;
        }
    }

    metrics.underlineThickness = face->underline_thickness;
    metrics.underlinePosition = face->underline_position + metrics.underlineThickness / 2; // dwrite: top of underline, freetype: center of underline

    if (os2)
    {
        metrics.strikethroughPosition = os2->yStrikeoutPosition;
        metrics.strikethroughThickness = os2->yStrikeoutSize;
    }
    else
    {
        metrics.strikethroughPosition = metrics.ascent / 3; // Approximation
        metrics.strikethroughThickness = metrics.lineGap / 2; // Approximation
    }

    std::println("FreeType: \n  Design Units Per Em: {}\n  Ascent: {}\n  Descent: {}\n  Line Gap: {}\n  Cap Height: {}\n  XHeight: {}\n  Underline Position: {}\n  Underline Thickness: {}\n  Strikethrough Position: {}\n  Strikethrough Thickness: {}",
                 metrics.designUnitsPerEm, metrics.ascent, metrics.descent, metrics.lineGap, metrics.capHeight, metrics.xHeight, metrics.underlinePosition, metrics.underlineThickness, metrics.strikethroughPosition, metrics.strikethroughThickness);

}

void dwrite()
{
    Microsoft::WRL::ComPtr<IDWriteFactory> factory;
    HRESULT hr = DWriteCreateFactory(
        DWRITE_FACTORY_TYPE_SHARED,
        __uuidof(IDWriteFactory),
        &factory
    );
    if (FAILED(hr))
    {
        std::println("Error creating DirectWrite factory: 0x{:X}", hr);
        return;
    }

    Microsoft::WRL::ComPtr<IDWriteFontCollection> fontCollection;
    hr = factory->GetSystemFontCollection(&fontCollection);
    if (FAILED(hr))
    {
        std::println("Error getting system font collection: 0x{:X}", hr);
        return;
    }

    UINT32 index;
    BOOL exists;
    hr = fontCollection->FindFamilyName(L"Arial", &index, &exists);
    if (FAILED(hr) || !exists)
    {
        std::println("Error finding font family: 0x{:X}", hr);
        return;
    }

    Microsoft::WRL::ComPtr<IDWriteFontFamily> fontFamily;
    hr = fontCollection->GetFontFamily(index, &fontFamily);
    if (FAILED(hr))
    {
        std::println("Error getting font family: 0x{:X}", hr);
        return;
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
        std::println("Error getting font: 0x{:X}", hr);
        return;
    }

    Microsoft::WRL::ComPtr<IDWriteFontFace> fontFace;
    hr = font->CreateFontFace(&fontFace);
    if (FAILED(hr))
    {
        std::println("Error creating font face: 0x{:X}", hr);
        return;
    }

    DWRITE_FONT_METRICS metrics = {};
    fontFace->GetMetrics(&metrics);
    std::println("DirectWrite: \n  Design Units Per Em: {}\n  Ascent: {}\n  Descent: {}\n  Line Gap: {}\n  Cap Height: {}\n  XHeight: {}\n  Underline Position: {}\n  Underline Thickness: {}\n  Strikethrough Position: {}\n  Strikethrough Thickness: {}",
                 metrics.designUnitsPerEm, metrics.ascent, metrics.descent, metrics.lineGap, metrics.capHeight, metrics.xHeight, metrics.underlinePosition, metrics.underlineThickness, metrics.strikethroughPosition, metrics.strikethroughThickness);

    UINT32 codePoint = character;
    UINT16 glyphIndex;
    hr = fontFace->GetGlyphIndicesW(&codePoint, 1, &glyphIndex);
    if (FAILED(hr))
    {
        std::println("Error getting glyph index: 0x{:X}", hr);
        return;
    }

    DWRITE_GLYPH_METRICS glyphMetrics = {};
    hr = fontFace->GetDesignGlyphMetrics(&glyphIndex, 1, &glyphMetrics);
    if (FAILED(hr))
    {
        std::println("Error getting glyph metrics: 0x{:X}", hr);
        return;
    }

    std::println("Glyph for '{}': \n  Advance X: {}\n  Bearing X: {}\n  Bearing Y: {}\n  Width: {}\n  Height: {}",
                 character,
                 glyphMetrics.advanceWidth,
                 glyphMetrics.leftSideBearing,
                 glyphMetrics.verticalOriginY - glyphMetrics.topSideBearing,
                 glyphMetrics.advanceWidth - glyphMetrics.leftSideBearing - glyphMetrics.rightSideBearing,
                 glyphMetrics.advanceHeight - glyphMetrics.topSideBearing - glyphMetrics.bottomSideBearing);
}

int main()
{
    freetype();
    // freetype_dwritelike();
    dwrite();

    return 0;
}