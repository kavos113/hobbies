#include <fstream>
#include <iostream>
#include <limits>

#include "src/color.h"
#include "src/hittable.h"
#include "src/hittable_list.h"
#include "src/vec3.h"
#include "src/ray.h"
#include "src/sphere.h"

constexpr double INFTY = std::numeric_limits<double>::infinity();

color3 ray_color(const ray& r, const hittable& obj)
{
    hit_record rec;
    if (obj.hit(r, 0.0, INFTY, rec)) {
        // If the ray hits an object, return a color based on the normal at the hit point
        return 0.5 * (rec.normal + color3(1.0, 1.0, 1.0));
    }
    // Background color: gradient from blue to white
    vec3 unit_direction = r.direction().unit();
    double t = 0.5 * (unit_direction.y() + 1.0);
    return (1.0 - t) * color3(1.0, 1.0, 1.0) + t * color3(0.5, 0.7, 1.0);
}

int main()
{
    constexpr double aspect_ratio = 16.0 / 9.0;
    constexpr int width = 512;
    constexpr int height = static_cast<int>(width / aspect_ratio);

    std::ofstream output("output.ppm");

    output << "P3\n" << width << " " << height << "\n255\n";

    double viewport_height = 2.0;
    double viewport_width = aspect_ratio * viewport_height;
    double focal_length = 1.0; // Distance from the camera to the viewport

    point3 origin(0, 0, 0);
    vec3 horizontal(viewport_width, 0, 0);
    vec3 vertical(0, viewport_height, 0);
    point3 lower_left_corner = origin - horizontal / 2 - vertical / 2 - vec3(0, 0, focal_length);

    hittable_list world;
    world.add(std::make_shared<sphere>(point3(0, 0, -1), 0.5));
    world.add(std::make_shared<sphere>(point3(0, -100.5, -1), 100));

    for (int y = height - 1; y >= 0; --y)
    {
        std::cout << "\rProgress: " << 100 - (y * 100) / (height - 1) << "%" << std::flush;
        for (int x = 0; x < width; ++x)
        {
            double u = static_cast<double>(x) / (width - 1);
            double v = static_cast<double>(y) / (height - 1);
            ray r(origin, lower_left_corner + u * horizontal + v * vertical - origin);

            color3 color = ray_color(r, world);

            print_color(output, color);
        }
    }
    output.close();

    std::cout << "\nImage generation complete. Output saved to output.ppm\n";

    return 0;
}