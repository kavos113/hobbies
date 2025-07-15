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

color3 ray_color(const ray& r, const hittable& obj, int depth)
{
    if (depth <= 0) {
        return color3(0, 0, 0);
    }

    hit_record rec;
    if (obj.hit(r, 0.0, INFTY, rec)) {
        // random diffuse vector
        point3 target = rec.p + rec.normal + vec3::random_in_unit_sphere();
        return 0.5 * ray_color(ray(rec.p, target - rec.p), obj, depth - 1);
    }
    // Background color: gradient from blue to white
    vec3 unit_direction = r.direction().unit();
    double t = 0.5 * (unit_direction.y() + 1.0);
    return (1.0 - t) * color3(1.0, 1.0, 1.0) + t * color3(0.5, 0.7, 1.0);
}

int main()
{
    constexpr double ASPECT = 16.0 / 9.0;
    constexpr int WIDTH = 512;
    constexpr int HEIGHT = static_cast<int>(WIDTH / ASPECT);
    constexpr int SAMPLES = 10;
    constexpr int MAX_DEPTH = 50;

    std::ofstream output("output5.ppm");

    output << "P3\n" << WIDTH << " " << HEIGHT << "\n255\n";

    hittable_list world;
    world.add(std::make_shared<sphere>(point3(0, 0, -1), 0.5));
    world.add(std::make_shared<sphere>(point3(0, -100.5, -1), 100));

    camera cam;

    for (int y = HEIGHT - 1; y >= 0; --y)
    {
        std::cout << "\rProgress: " << 100 - (y * 100) / (HEIGHT - 1) << "%" << std::flush;
        for (int x = 0; x < WIDTH; ++x)
        {
            color3 color(0, 0, 0);

            for (int s = 0; s < SAMPLES; ++s)
            {
                double u = (x + random_double()) / (WIDTH - 1);
                double v = (y + random_double()) / (HEIGHT - 1);
                ray r = cam.get_ray(u, v);
                color += ray_color(r, world, MAX_DEPTH);
            }

            print_color(output, color, SAMPLES);
        }
    }
    output.close();

    std::cout << "\nImage generation complete. Output saved to output.ppm\n";

    return 0;
}