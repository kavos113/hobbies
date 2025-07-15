#include <fstream>
#include <iostream>

#include "src/color.h"
#include "src/vec3.h"
#include "src/ray.h"

// ray = a + tb (origin = a, direction = b)
// (b . b) t^2 + 2tb . (a - c) + (a - c) . (a - c) - r^2 = 0
double hit_sphere(const point3& center, double radius, const ray& r)
{
    vec3 oc = r.origin() - center; // a - c
    double a = r.direction().squared_length(); // b . b
    double half_b = dot(oc, r.direction()); // b . (a - c)
    double c = oc.squared_length() - radius * radius; // (a - c) . (a - c) - r^2
    double discriminant = half_b * half_b - a * c; // b^2 - ac
    if (discriminant < 0) {
        return -1.0; // No intersection
    } else {
        return (-half_b - std::sqrt(discriminant)) / a; // Return the nearest root
    }
}

color3 ray_color(const ray& r)
{
    point3 center(0, 0, -1); // Center of the sphere
    double t = hit_sphere(center, 0.5, r);
    if (t > 0.0) {
        vec3 normal = (r.at(t) - center).unit(); // Normal at the intersection point. (x - c) / |x - c|
        return 0.5 * color3(normal.x() + 1, normal.y() + 1, normal.z() + 1);
    }

    vec3 unit_direction = r.direction().unit();
    t = 0.5 * (unit_direction.y() + 1.0);
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

    for (int y = height - 1; y >= 0; --y)
    {
        std::cout << "\rProgress: " << 100 - (y * 100) / (height - 1) << "%" << std::flush;
        for (int x = 0; x < width; ++x)
        {
            double u = static_cast<double>(x) / (width - 1);
            double v = static_cast<double>(y) / (height - 1);
            ray r(origin, lower_left_corner + u * horizontal + v * vertical - origin);
            color3 color = ray_color(r);
            print_color(output, color);
        }
    }
    output.close();

    std::cout << "\nImage generation complete. Output saved to output.ppm\n";

    return 0;
}