#include <fstream>
#include <iostream>

#include "color.h"
#include "vec3.h"

int main()
{
    constexpr int width = 256;
    constexpr int height = 256;

    std::ofstream output("output.ppm");

    output << "P3\n" << width << " " << height << "\n255\n";

    for (int y = 0; y < height; ++y) {
        std::cout << "\rProgress: " << (y * 100) / (height - 1) << "%" << std::flush;
        for (int x = 0; x < width; ++x) {
            color3 color(
                static_cast<double>(x) / (width - 1),
                static_cast<double>(y) / (height - 1),
                0.2
            );
            print_color(output, color);
        }
    }
    output.close();

    std::cout << "\nImage generation complete. Output saved to output.ppm\n";

    return 0;
}