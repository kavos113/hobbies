#include <fstream>
#include <iostream>
#include <limits>
#include <memory>
#include <chrono>
#include <omp.h>

#include "src/camera.h"
#include "src/color.h"
#include "src/hittable.h"
#include "src/hittable_list.h"
#include "src/vec3.h"
#include "src/ray.h"
#include "src/sphere.h"
#include "src/util.h"
#include "src/material.h"
#include "src/lambert.h"
#include "src/metal.h"

constexpr double INFTY = std::numeric_limits<double>::infinity();

color3 ray_color(const ray& r, const hittable& obj, int depth)
{
    if (depth <= 0) {
        return color3(0, 0, 0);
    }

    hit_record rec;
    if (obj.hit(r, 0.001, INFTY, rec)) {
        ray scattered;
        color3 attenuation;
        if (rec.mat->scatter(r, rec, attenuation, scattered)) {
            return attenuation * ray_color(scattered, obj, depth - 1);
        }
        return color3(0, 0, 0); // No scattering, return black
    }
    // Background color: gradient from blue to white
    vec3 unit_direction = r.direction().unit();
    double t = 0.5 * (unit_direction.y() + 1.0);
    return (1.0 - t) * color3(1.0, 1.0, 1.0) + t * color3(0.5, 0.7, 1.0);
}

void output(const std::vector<color3>& image, int width, int height, int samples, const std::string& filename)
{
    std::ofstream output(filename);
    if (!output) {
        std::cerr << "Error opening file for output: " << filename << std::endl;
        return;
    }

    output << "P3\n" << width << " " << height << "\n255\n";

    for (int y = height - 1; y >= 0; --y) {
        for (int x = 0; x < width; ++x) {
            const color3& c = image[y * width + x];
            print_color(output, c, samples);
        }
    }

    output.close();
}

int main()
{
    constexpr double ASPECT = 16.0 / 9.0;
    constexpr int WIDTH = 512;
    constexpr int HEIGHT = static_cast<int>(WIDTH / ASPECT);
    constexpr int SAMPLES = 100;
    constexpr int MAX_DEPTH = 50;

    auto start_time = std::chrono::high_resolution_clock::now();

    hittable_list world;
    world.add(std::make_shared<sphere>(point3(0, 0, -1), 0.5, std::make_shared<lambert>(color3(0.4, 0.6, 0.7))));
    world.add(std::make_shared<sphere>(point3(0, -100.5, -1), 100, std::make_shared<lambert>(color3(0.8, 0.8, 0.0))));
    world.add(std::make_shared<sphere>(point3(1, 0, -1), 0.5, std::make_shared<metal>(color3(0.8, 0.6, 0.2))));
    world.add(std::make_shared<sphere>(point3(-1, 0, -1), 0.5, std::make_shared<metal>(color3(0.8, 0.8, 0.8))));

    camera cam;

    std::vector<color3> image(WIDTH * HEIGHT);

    for (int y = HEIGHT - 1; y >= 0; --y)
    {
        std::cout << "\rProgress: " << 100 - (y * 100) / (HEIGHT - 1) << "%" << std::flush;
#pragma omp parallel for
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

            image[y * WIDTH + x] = color;
        }
    }

    output(image, WIDTH, HEIGHT, SAMPLES, "output9.ppm");

    std::cout << "\nImage generation complete. Output saved to output.ppm\n";

    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    std::cout << "Time taken: " << duration.count() << " ms\n";

    return 0;
}