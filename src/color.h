#ifndef SRC_COLOR_H
#define SRC_COLOR_H

#include <iostream>
#include <algorithm>

#include "vec3.h"

inline void print_color(std::ostream &out, const color3& c, int samples_per_pixel = 1){
    double r = c.x();
    double g = c.y();
    double b = c.z();

    double scale = 1.0 / samples_per_pixel;
    r = std::sqrt(scale * r);
    g = std::sqrt(scale * g);
    b = std::sqrt(scale * b);

    out << static_cast<int>(255.999 * std::ranges::clamp(r, 0.0, 0.999)) << ' '
        << static_cast<int>(255.999 * std::ranges::clamp(g, 0.0, 0.999)) << ' '
        << static_cast<int>(255.999 * std::ranges::clamp(b, 0.0, 0.999)) << '\n';
}

#endif //SRC_COLOR_H
