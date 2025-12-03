#include <pango/pangocairo.h>
#include <iostream>

int main() {
    int width = 400;
    int height = 200;
    const char *filename = "pango_sample.png";

    cairo_surface_t *surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);
    cairo_t *cr = cairo_create(surface);

    cairo_set_source_rgb(cr, 1.0, 1.0, 0.5);
    cairo_paint(cr);

    PangoLayout *layout = pango_cairo_create_layout(cr);

    std::string text = "This is a sample pango text. 日本語が混じっていても問題ないです。";
    std::string short_text = "Short text";

    PangoFontDescription *desc = pango_font_description_from_string("Meiryo UI Medium 20");
    pango_layout_set_font_description(layout, desc);
    pango_font_description_free(desc);

    pango_layout_set_text(layout, short_text.c_str(), -1);

    pango_layout_set_width(layout, width * PANGO_SCALE);
    pango_layout_set_alignment(layout, PANGO_ALIGN_CENTER);
    pango_layout_set_line_spacing(layout, 1.5);

    cairo_set_source_rgb(cr, 0.0, 0.0, 0.0); // 黒

    cairo_move_to(cr, 0, 0);
    pango_cairo_show_layout(cr, layout);

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