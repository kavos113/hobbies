#ifndef LINEAR_SRC_QUARTANION_H
#define LINEAR_SRC_QUARTANION_H

#include <cmath>

template <typename T>
class quartanion
{
public:
    T w, x, y, z;

    quartanion(T w = 1, T x = 0, T y = 0, T z = 0) : w(w), x(x), y(y), z(z) {}

    // (q, qw) * (r, rw) = (q x r + rw * q + qw * r, qw * rw - q . r)
    quartanion operator*(const quartanion& r) const {
        return quartanion(
            w * r.w - x * r.x - y * r.y - z * r.z,
            w * r.x + x * r.w + y * r.z - z * r.y,
            w * r.y + y * r.w + z * r.x - x * r.z,
            w * r.z + z * r.w + x * r.y - y * r.x
        );
    }

    quartanion operator*(const T& k) const {
        return quartanion(w * k, x * k, y * k, z * k);
    }

    quartanion operator/(const T& k) const {
        return quartanion(w / k, x / k, y / k, z / k);
    }

    quartanion operator+(const quartanion& other) const {
        return quartanion(w + other.w, x + other.x, y + other.y, z + other.z);
    }

    quartanion operator-(const quartanion& other) const {
        return quartanion(w - other.w, x - other.x, y - other.y, z - other.z);
    }

    quartanion conjugate() const {
        return quartanion(w, -x, -y, -z);
    }

    T norm() const {
        return std::sqrt(w*w + x*x + y*y + z*z);
    }

    quartanion inverse() const {
        T n = norm();
        if (n == 0) return quartanion(0, 0, 0, 0);
        return conjugate() / (n * n);
    }

    quartanion operator/(const quartanion& other) const {
        return *this * other.inverse();
    }
};

#endif //LINEAR_SRC_QUARTANION_H