#ifndef SRC_METAL_H
#define SRC_METAL_H

#include "material.h"

class metal : public material
{
public:
    metal(const color3& albedo, double fuzz) : albedo(albedo), fuzz(fuzz < 1 ? fuzz : 1) {}

    bool scatter(
        const ray& r_in,
        const hit_record& rec,
        color3& attenuation,
        ray& scattered
    ) const override
    {
        vec3 reflected = vec3::reflect(r_in.direction(), rec.normal).unit();
        scattered = ray(rec.p, reflected + fuzz * vec3::random_in_unit_sphere());
        attenuation = albedo;
        return vec3::dot(scattered.direction(), rec.normal) > 0;
    }

private:
    color3 albedo;
    double fuzz;
};



#endif //SRC_METAL_H
