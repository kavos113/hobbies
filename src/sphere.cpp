#include "sphere.h"

#include <cmath>

// ray = a + tb (origin = a, direction = b)
// (b . b) t^2 + 2tb . (a - c) + (a - c) . (a - c) - r^2 = 0
bool sphere::hit(const ray& r, double t_min, double t_max, hit_record& rec) const
{
    vec3 oc = r.origin() - m_center; // a - c
    double a = r.direction().squared_length(); // b . b
    double half_b = vec3::dot(oc, r.direction()); // b . (a - c)
    double c = oc.squared_length() - m_radius * m_radius; // (a - c) . (a - c) - r^2
    double discriminant = half_b * half_b - a * c; // b^2 - ac

    if (discriminant < 0) {
        return false; // No intersection
    }

    double t = (-half_b - std::sqrt(discriminant)) / a; // Return the nearest root
    if (t < t_max && t > t_min) {
        rec.t = t;
        rec.p = r.at(t);
        vec3 out_normal = (rec.p - m_center).unit(); // Normal at the intersection point. (x - c) / |x - c|
        rec.set_face_normal(r, out_normal);
        return true;
    }
    t = (-half_b + std::sqrt(discriminant)) / a; // Check the second root
    if (t < t_max && t > t_min) {
        rec.t = t;
        rec.p = r.at(t);
        vec3 out_normal = (rec.p - m_center).unit();
        rec.set_face_normal(r, out_normal);
        return true;
    }

    return false; // No valid intersection
}
