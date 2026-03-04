#ifndef LINEAR_SRC_QUARTANION_H
#define LINEAR_SRC_QUARTANION_H

#include <cmath>
#include <iostream>

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

    quartanion operator-() const {
        return quartanion(-w, -x, -y, -z);
    }

    quartanion operator+() const {
        return *this;
    }

    quartanion operator*(const T& k) {
        return quartanion(w * k, x * k, y * k, z * k);
    }

    quartanion operator/(const T& k) {
        return quartanion(w / k, x / k, y / k, z / k);
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

    bool operator==(const quartanion& other) const {
        return w == other.w && x == other.x && y == other.y && z == other.z;
    }
};

template <typename T>
quartanion<T> operator*(const T& k, const quartanion<T>& q) {
    return quartanion<T>(q.w * k, q.x * k, q.y * k, q.z * k);
}

template <typename T>
quartanion<T> operator/(const T& k, const quartanion<T>& q) {
    return quartanion<T>(q.w * k, q.x * k, q.y * k, q.z * k) / q.norm();
}

template <typename CharT, typename Traits, typename T>
std::basic_ostream<CharT, Traits>&
operator<<(std::basic_ostream<CharT, Traits>& os, const quartanion<T>& q) {
    os << q.x << "i + " << q.y << "j + " << q.z << "k + " << q.w;
    return os;
}

#endif //LINEAR_SRC_QUARTANION_H