#include <ft2build.h>
#include FT_FREETYPE_H

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

    error = FT_Set_Char_Size(face, 0, 512 * 64, 300, 300); // width: same as height
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

    FT_Done_Face(face);
    FT_Done_FreeType(library);
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
}

int main()
{
    freetype();
    dwrite();

    return 0;
}