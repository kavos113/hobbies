#ifndef SRC_RAY_H
#define SRC_RAY_H

#include "vec3.h"

// P(t) = Origin + t * Dir
class ray
{
public:
    ray() {}
    ray(const point3& origin, const vec3& direction)
        : m_origin(origin), m_dir(direction) {}

    point3 origin() const { return m_origin; }
    vec3 direction() const { return m_dir; }

    point3 at(double t) const
    {
        return m_origin + t * m_dir;
    }
private:
    point3 m_origin;
    vec3 m_dir;
};



#endif //SRC_RAY_H
