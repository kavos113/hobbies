#include <pango/pangocairo.h>
#include <iostream>

int main()
{
    int width = 400;
    int height = 200;
    const char *filename = "pango_sample.png";

    cairo_surface_t *surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);
    cairo_t *cr = cairo_create(surface);

    cairo_set_source_rgb(cr, 1.0, 1.0, 0.5);
    cairo_paint(cr);

    PangoLayout *layout = pango_cairo_create_layout(cr);

    PangoContext *context = pango_layout_get_context(layout);
    PangoGravity g = pango_context_get_base_gravity(context);
    std::cout << "Gravity: " << g << std::endl;
    pango_context_set_base_gravity(context, PANGO_GRAVITY_EAST);
    pango_context_set_gravity_hint(context, PANGO_GRAVITY_HINT_STRONG);

    PangoLanguage* lang = pango_language_from_string("ja");
    pango_context_set_language(context, lang);

    pango_layout_context_changed(layout);

    PangoGravity gravity = pango_context_get_base_gravity(context);
    std::cout << "Gravity: " << gravity << std::endl;

    std::string text = "縦書きテキストのサンプルです。あああああああああああああああEnglishEnglish";

    PangoFontDescription *desc = pango_font_description_from_string("MS Gothic 10");
    pango_layout_set_font_description(layout, desc);
    pango_font_description_free(desc);

    pango_layout_set_width(layout, -1);
    pango_layout_set_height(layout, PANGO_SCALE * height);
    pango_layout_set_text(layout, text.c_str(), -1);

    cairo_set_source_rgb(cr, 0.0, 0.0, 0.0);
    cairo_move_to(cr, width - 50, 20);
    pango_cairo_show_layout(cr, layout);

    context = pango_layout_get_context(layout);
    PangoGravity new_gravity = pango_context_get_gravity(context);
    std::cout << "New Gravity: " << new_gravity << std::endl;

    g_object_unref(layout);
    cairo_destroy(cr);

    if (cairo_surface_write_to_png(surface, filename) != CAIRO_STATUS_SUCCESS) {
        fprintf(stderr, "PNGファイルの保存に失敗しました。\n");
        cairo_surface_destroy(surface);
        return 1;
    }

    printf("'%s' を生成しました。\n", filename);
    cairo_surface_destroy(surface);

    return 0;
}