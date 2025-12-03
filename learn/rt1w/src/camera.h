#ifndef SRC_CAMERA_H
#define SRC_CAMERA_H

#include "vec3.h"
#include "ray.h"

class camera
{
public:
    camera(
        point3 lookfrom,
        point3 lookat,
        vec3 up,
        double fov, //radians
        double aspect
    )
    {
        double h = std::tan(fov / 2.0);
        double viewport_height = 2.0 * h;
        double viewport_width = aspect * viewport_height;

        vec3 w = (lookfrom - lookat).unit();
        vec3 u = vec3::cross(up, w).unit();
        vec3 v = vec3::cross(w, u);

        m_origin = lookfrom;
        m_horizontal = viewport_width * u;
        m_vertical = viewport_height * v;
        m_lower_left_corner = m_origin - m_horizontal / 2.0 - m_vertical / 2.0 - w;
    }

    ray get_ray(double u, double v) const
    {
        return ray(m_origin, m_lower_left_corner + u * m_horizontal + v * m_vertical - m_origin);
    }

private:
    point3 m_origin;
    point3 m_lower_left_corner;
    vec3 m_horizontal;
    vec3 m_vertical;
};

#endif //SRC_CAMERA_H
