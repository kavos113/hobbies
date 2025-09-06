#include <ft2build.h>
#include FT_FREETYPE_H

#include <print>
#include <string>
#include <vector>

struct glyph_info
{
    std::vector<unsigned char> bitmap_buffer;
    unsigned int width;
    unsigned int height;
    int left;
    int top;
    int advance_x;
};

int main()
{
    std::string text = "Hello, FreeType!";

    FT_Library library;

    FT_Error error = FT_Init_FreeType(&library);
    if (error)
    {
        std::println("Error initializing FreeType: {}", error);
        return 1;
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
        return 1;
    }
    if (error)
    {
        std::println("Error loading font: {}", error);
        FT_Done_FreeType(library);
        return 1;
    }

    error = FT_Set_Char_Size(face, 0, 16 * 64, 300, 300); // width: same as height
    if (error)
    {
        std::println("Error setting character size: {}", error);
        FT_Done_Face(face);
        FT_Done_FreeType(library);
        return 1;
    }

    std::vector<glyph_info> glyphs(text.size());

    for (size_t i = 0; i < text.size(); ++i)
    {
        FT_UInt glyph_index = FT_Get_Char_Index(face, static_cast<FT_ULong>(text[i]));

        error = FT_Load_Glyph(face, glyph_index, FT_LOAD_DEFAULT);
        if (error)
        {
            std::println("Error loading glyph for character '{}': {}", text[i], error);
            continue;
        }

        error = FT_Render_Glyph(face->glyph, FT_RENDER_MODE_NORMAL);
        if (error)
        {
            std::println("Error rendering glyph for character '{}': {}", text[i], error);
            continue;
        }

        FT_GlyphSlot slot = face->glyph;

        glyphs[i].width = slot->bitmap.width;
        glyphs[i].height = slot->bitmap.rows;
        glyphs[i].left = slot->bitmap_left;
        glyphs[i].top = slot->bitmap_top;
        glyphs[i].advance_x = slot->advance.x >> 6; // Convert from 26.6 fixed point to integer

        size_t buffer_size = slot->bitmap.width * slot->bitmap.rows;
        glyphs[i].bitmap_buffer.resize(buffer_size);
        std::copy_n(slot->bitmap.buffer, buffer_size, glyphs[i].bitmap_buffer.begin());
    }

    int top = 0;
    int bottom = 0;
    for (const auto& glyph : glyphs)
    {
        if (glyph.top > top)
            top = glyph.top;
        int glyph_bottom = glyph.top - static_cast<int>(glyph.height);
        if (glyph_bottom < bottom)
            bottom = glyph_bottom;
    }

    std::println("Rendering text: '{}', top = {}, bottom = {}", text, top, bottom);

    for (int row = 0; row <= top - bottom; ++row)
    {
        for (const auto& glyph : glyphs)
        {
            int glyph_row = row - top + glyph.top;

            if (glyph_row < 0 || glyph_row >= static_cast<int>(glyph.height))
            {
                for (unsigned int col = 0; col < glyph.advance_x; ++col)
                    std::print(" ");
            }
            else
            {
                for (unsigned int col = 0; col < glyph.width; ++col)
                {
                    unsigned char pixel_value = glyph.bitmap_buffer[glyph_row * glyph.width + col];
                    if (pixel_value > 128) // threshold for visibility
                        std::print("#");
                    else
                        std::print(" ");
                }
                for (unsigned int col = glyph.width; col < static_cast<unsigned int>(glyph.advance_x); ++col)
                    std::print(" ");
            }
        }
        std::println();
    }

    FT_Done_Face(face);
    FT_Done_FreeType(library);

    return 0;
}