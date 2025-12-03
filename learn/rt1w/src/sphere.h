#ifndef SRC_SPHERE_H
#define SRC_SPHERE_H

#include "hittable.h"

class sphere : public hittable
{
public:
    sphere() = default;
    sphere(const point3& center, double radius, std::shared_ptr<material> mat)
        : m_center(center), m_radius(radius), m_material(std::move(mat)) {}

    bool hit(const ray& r, double t_min, double t_max, hit_record& rec) const override;

    point3 center() const { return m_center; }
    double radius() const { return m_radius; }

private:
    point3 m_center;
    double m_radius;
    std::shared_ptr<material> m_material;
};

#endif //SRC_SPHERE_H
