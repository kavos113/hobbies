#include <fstream>
#include <iostream>
#include <limits>
#include <memory>
#include <chrono>
#include <omp.h>

#include "src/camera.h"
#include "src/color.h"
#include "src/dielectric.h"
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

hittable_list random_scene(int count)
{
    hittable_list world;

    auto ground_material = std::make_shared<lambert>(color3(0.5, 0.5, 0.5));
    world.add(std::make_shared<sphere>(point3(0, -1000, 0), 1000, ground_material));

    for (int x = -count; x < count; ++x)
    {
        for (int z = -count; z < count; ++z)
        {
            double choose_mat = random_double();
            point3 center(x + 0.9 * random_double(), 0.2, z + 0.9 * random_double());

            if ((center - point3(4, 0.2, 0)).length() > 0.9)
            {
                std::shared_ptr<material> mat;
                if (choose_mat < 0.8) {
                    // diffuse
                    color3 albedo = color3::random() * color3::random();
                    mat = std::make_shared<lambert>(albedo);
                } else if (choose_mat < 0.95) {
                    // metal
                    color3 albedo = color3::random(0.5, 1);
                    double fuzz = random_double(0, 0.5);
                    mat = std::make_shared<metal>(albedo, fuzz);
                } else {
                    // glass
                    mat = std::make_shared<dielectric>(1.5);
                }
                world.add(std::make_shared<sphere>(center, 0.2, mat));
            }
        }
    }

    world.add(std::make_shared<sphere>(point3(0, 1, 0), 1.0, std::make_shared<dielectric>(1.5)));
    world.add(std::make_shared<sphere>(point3(-4, 1, 0), 1.0, std::make_shared<lambert>(color3(0.4, 0.2, 0.1))));
    world.add(std::make_shared<sphere>(point3(4, 1, 0), 1.0, std::make_shared<metal>(color3(0.7, 0.6, 0.5), 0.0)));

    return world;
}

int main()
{
    constexpr double ASPECT = 16.0 / 9.0;
    constexpr int WIDTH = 512;
    constexpr int HEIGHT = static_cast<int>(WIDTH / ASPECT);
    constexpr int SAMPLES = 100;
    constexpr int MAX_DEPTH = 50;

    auto start_time = std::chrono::high_resolution_clock::now();

    hittable_list world = random_scene(11);

    camera cam(
        point3(6, 2, 3),
        point3(0, 0, 0),
        vec3(0, 1, 0),
        std::numbers::pi / 2.0, // Field of view in radians
        ASPECT // Aspect ratio
    );

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

    output(image, WIDTH, HEIGHT, SAMPLES, "output14.ppm");

    std::cout << "\nImage generation complete. Output saved to output.ppm\n";

    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    std::cout << "Time taken: " << duration.count() << " ms\n";

    return 0;
}