#ifndef SRC_DIELECTRIC_H
#define SRC_DIELECTRIC_H

#include "material.h"

class dielectric : public material
{
public:
    dielectric(double ref_idx) : m_ref_idx(ref_idx) {}

    bool scatter(
        const ray& r_in,
        const hit_record& rec,
        color3& attenuation,
        ray& scattered
    ) const override
    {
        attenuation = color3(1.0, 1.0, 1.0);
        double ref_index;
        if (rec.front_face) {
            ref_index = 1.0 / m_ref_idx;
        } else {
            ref_index = m_ref_idx;
        }

        vec3 unit_direction = r_in.direction().unit();

        double cos_theta = std::fmin(vec3::dot(-unit_direction, rec.normal), 1.0);
        double sin_theta = std::sqrt(1.0 - cos_theta * cos_theta);
        if (ref_index * sin_theta > 1.0) {
            // Total internal reflection
            vec3 reflected = vec3::reflect(unit_direction, rec.normal);
            scattered = ray(rec.p, reflected);
            return true;
        }

        vec3 refracted = refract(unit_direction, rec.normal, ref_index);
        scattered = ray(rec.p, refracted);
        return true;
    }

private:
    double m_ref_idx;
};



#endif //SRC_DIELECTRIC_H
