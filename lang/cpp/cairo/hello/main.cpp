#include <cairo.h>
#include <iostream>
#include <numbers>

int main() {
    std::cout << "hello cairo" << std::endl;

    int width = 400;
    int height = 200;
    const char *filename = "cairo_sample.png";

    cairo_surface_t *surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);

    cairo_t *cr = cairo_create(surface);

    cairo_set_source_rgb(cr, 1.0, 1.0, 1.0);
    cairo_paint(cr);

    cairo_set_source_rgb(cr, 1.0, 0.0, 0.0); // red
    cairo_rectangle(cr, 20, 20, 100, 60);
    cairo_fill(cr);

    cairo_set_source_rgb(cr, 0.0, 0.0, 1.0); // blue
    cairo_set_line_width(cr, 4.0);
    cairo_move_to(cr, 150, 40);
    cairo_line_to(cr, 250, 100);
    cairo_stroke(cr);

    cairo_set_source_rgb(cr, 0.0, 0.8, 0.0); // green
    cairo_set_line_width(cr, 6.0);
    cairo_arc(cr, 300, 70, 50, 0, 2 * std::numbers::pi);
    cairo_stroke(cr);

    cairo_destroy(cr);

    if (cairo_surface_write_to_png(surface, filename) != CAIRO_STATUS_SUCCESS) {
        std::cerr << "PNGファイルの保存に失敗しました。" << std::endl;
        cairo_surface_destroy(surface);
        return 1;
    }

    std::cout << "描画が完了しました。ファイル名: " << filename << std::endl;

    cairo_surface_destroy(surface);

    return 0;
}