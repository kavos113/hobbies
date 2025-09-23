#include <ft2build.h>
#include FT_FREETYPE_H

#include <hb.h>
#include <hb-ft.h>

#include <print>
#include <string>
#include <vector>

typedef struct
{
    std::vector<unsigned char> bitmap_buffer;
    unsigned int width;
    unsigned int height;
    int left;
    int top;
    int advance_x;
} GlyphInfo;

int main()
{
    std::string text = "final illustration";

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

    error = FT_Set_Pixel_Sizes(face, 0, 24);
    if (error)
    {
        std::println("Error setting character size: {}", error);
        FT_Done_Face(face);
        FT_Done_FreeType(library);
        return 1;
    }

    hb_font_t *hb_font = hb_ft_font_create_referenced(face);
    hb_buffer_t *hb_buffer = hb_buffer_create();
    hb_buffer_add_utf8(hb_buffer, text.c_str(), -1, 0, -1);

    hb_buffer_set_direction(hb_buffer, HB_DIRECTION_LTR);
    hb_buffer_set_script(hb_buffer, HB_SCRIPT_LATIN);
    hb_buffer_set_language(hb_buffer, hb_language_from_string("en", -1));

    hb_shape(hb_font, hb_buffer, nullptr, 0);

    unsigned int glyph_count;
    hb_glyph_info_t *glyph_info = hb_buffer_get_glyph_infos(hb_buffer, &glyph_count);
    hb_glyph_position_t *glyph_pos = hb_buffer_get_glyph_positions(hb_buffer, &glyph_count);

    float pen_x = 0;
    float pen_y = 0;

    std::vector<GlyphInfo> glyphs;
    glyphs.reserve(glyph_count);

    for (unsigned int i = 0; i < glyph_count; i++)
    {
        FT_UInt glyph_index = glyph_info[i].codepoint;

        error = FT_Load_Glyph(face, glyph_index, FT_LOAD_DEFAULT);
        if (error)
        {
            std::println("Error loading glyph index {}: {}", glyph_index, error);
            continue;
        }

        error = FT_Render_Glyph(face->glyph, FT_RENDER_MODE_NORMAL);
        if (error)
        {
            std::println("Error rendering glyph index {}: {}", glyph_index, error);
            continue;
        }

        FT_GlyphSlot slot = face->glyph;

        GlyphInfo g_info;
        g_info.width = slot->bitmap.width;
        g_info.height = slot->bitmap.rows;
        g_info.left = slot->bitmap_left;
        g_info.top = slot->bitmap_top;
        g_info.advance_x = slot->advance.x >> 6; // Convert from 26.6 fixed point to integer

        g_info.bitmap_buffer.resize(g_info.width * g_info.height);
        std::memcpy(g_info.bitmap_buffer.data(), slot->bitmap.buffer, g_info.bitmap_buffer.size());

        // Here you can use g_info as needed
        std::println("Glyph {}: size={}x{}, left={}, top={}, advance_x={}",
                     glyph_index, g_info.width, g_info.height, g_info.left, g_info.top, g_info.advance_x);

        pen_x += (glyph_pos[i].x_advance >> 6); // Convert from 26.6 fixed point to integer
        pen_y += (glyph_pos[i].y_advance >> 6);

        glyphs.push_back(std::move(g_info));
    }

    std::println("Total pen position after rendering: x={}, y={}", pen_x, pen_y);

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

    hb_buffer_destroy(hb_buffer);
    hb_font_destroy(hb_font);

    FT_Done_Face(face);
    FT_Done_FreeType(library);

    return 0;
}