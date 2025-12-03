#ifndef SRC_MATERIAL_H
#define SRC_MATERIAL_H

#include "ray.h"
#include "hittable.h"

class material
{
public:
    virtual ~material() = default;

    virtual bool scatter(
        const ray& r_in,
        const hit_record& rec,
        color3& attenuation,
        ray& scattered
    ) const = 0;
};



#endif //SRC_MATERIAL_H
