#include <fstream>
#include <iostream>
#include <limits>

#include "src/camera.h"
#include "src/color.h"
#include "src/hittable.h"
#include "src/hittable_list.h"
#include "src/vec3.h"
#include "src/ray.h"
#include "src/sphere.h"
#include "src/util.h"

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
    constexpr int samples_per_pixel = 10;

    std::ofstream output("output4.ppm");

    output << "P3\n" << width << " " << height << "\n255\n";

    hittable_list world;
    world.add(std::make_shared<sphere>(point3(0, 0, -1), 0.5));
    world.add(std::make_shared<sphere>(point3(0, -100.5, -1), 100));

    camera cam;

    for (int y = height - 1; y >= 0; --y)
    {
        std::cout << "\rProgress: " << 100 - (y * 100) / (height - 1) << "%" << std::flush;
        for (int x = 0; x < width; ++x)
        {
            color3 color(0, 0, 0);

            for (int s = 0; s < samples_per_pixel; ++s)
            {
                double u = (x + random_double()) / (width - 1);
                double v = (y + random_double()) / (height - 1);
                ray r = cam.get_ray(u, v);
                color += ray_color(r, world);
            }

            print_color(output, color, samples_per_pixel);
        }
    }
    output.close();

    std::cout << "\nImage generation complete. Output saved to output.ppm\n";

    return 0;
}