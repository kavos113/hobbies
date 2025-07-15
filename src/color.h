#ifndef SRC_COLOR_H
#define SRC_COLOR_H

#include <iostream>

#include "vec3.h"

inline void print_color(std::ostream &out, const color3& c) {
    out << static_cast<int>(255.999 * c.x()) << ' '
        << static_cast<int>(255.999 * c.y()) << ' '
        << static_cast<int>(255.999 * c.z()) << '\n';
}

#endif //SRC_COLOR_H
