#ifndef SRC_LAMBERT_H
#define SRC_LAMBERT_H

#include "material.h"
#include "vec3.h"


class lambert : public material
{
public:
    lambert(const color3& albedo) : albedo(albedo) {}

    bool scatter(
        const ray& r_in,
        const hit_record& rec,
        color3& attenuation,
        ray& scattered
    ) const override
    {
        vec3 scattered_direction = rec.normal  + vec3::random_unit_vector();
        scattered = ray(rec.p, scattered_direction);
        attenuation = albedo;
        return true;
    }
private:
    color3 albedo;
};



#endif //SRC_LAMBERT_H
