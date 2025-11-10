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

    const char *markup_text =
        "<span font_desc='Sans Bold 24' foreground='blue'>Pango サンプル</span>\n"
        "これは <b>太字</b>、<i>斜体</i>、<u>下線</u>、<span foreground='red'>色付き</span> テキストです。\n"
        "<small>小さいテキスト</small> や <big>大きいテキスト</big> も扱えます。\n"
        "もちろん日本語とEnglishが混在しても、Pangoが適切に処理します。";

    pango_layout_set_markup(layout, markup_text, -1);

    pango_layout_set_width(layout, (width - 20) * PANGO_SCALE);

    cairo_set_source_rgb(cr, 0.0, 0.0, 0.0); // 黒

    cairo_move_to(cr, 10, 10);
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