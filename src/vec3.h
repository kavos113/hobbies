#ifndef VEC3_H
#define VEC3_H

#include <array>
#include <cmath>
#include <numbers>

#include "util.h"

class vec3
{
public:
    vec3() : data{0.0f, 0.0f, 0.0f} {}
    vec3(double x, double y, double z) : data{x, y, z} {}

    double x() const { return data[0]; }
    double y() const { return data[1]; }
    double z() const { return data[2]; }

    vec3 operator-() const {
        return {-data[0], -data[1], -data[2]};
    }

    double operator[](int index) const {
        return data[index];
    }

    double& operator[](int index) {
        return data[index];
    }

    vec3 operator+(const vec3& other) const {
        return {data[0] + other.data[0], data[1] + other.data[1], data[2] + other.data[2]};
    }

    vec3 operator-(const vec3& other) const {
        return {data[0] - other.data[0], data[1] - other.data[1], data[2] - other.data[2]};
    }

    vec3 operator*(double scalar) const {
        return {data[0] * scalar, data[1] * scalar, data[2] * scalar};
    }

    vec3 operator*(const vec3& other) const {
        return {data[0] * other.data[0], data[1] * other.data[1], data[2] * other.data[2]};
    }

    vec3 operator/(double scalar) const {
        if (scalar != 0.0f) {
            return {data[0] / scalar, data[1] / scalar, data[2] / scalar};
        }
        return {0.0f, 0.0f, 0.0f}; // Handle division by zero
    }

    vec3& operator+=(const vec3& other) {
        data[0] += other.data[0];
        data[1] += other.data[1];
        data[2] += other.data[2];
        return *this;
    }

    vec3& operator-=(const vec3& other) {
        data[0] -= other.data[0];
        data[1] -= other.data[1];
        data[2] -= other.data[2];
        return *this;
    }

    vec3& operator*=(double scalar) {
        data[0] *= scalar;
        data[1] *= scalar;
        data[2] *= scalar;
        return *this;
    }

    vec3& operator/=(double scalar) {
        if (scalar != 0.0f) {
            data[0] /= scalar;
            data[1] /= scalar;
            data[2] /= scalar;
        }
        return *this;
    }

    double length() const {
        return std::sqrt(data[0] * data[0] + data[1] * data[1] + data[2] * data[2]);
    }

    double squared_length() const {
        return data[0] * data[0] + data[1] * data[1] + data[2] * data[2];
    }

    vec3 unit() const {
        double len = length();
        if (len > 0.0f) {
            return *this / len;
        }
        return {0.0f, 0.0f, 0.0f}; // Return zero vector if length is zero
    }

    static vec3 random() {
        return {random_double(), random_double(), random_double()};
    }

    static vec3 random(double min, double max) {
        return {random_double(min, max), random_double(min, max), random_double(min, max)};
    }

    static vec3 random_in_unit_sphere() {
        while (true) {
            vec3 p = random(-1.0, 1.0);
            if (p.squared_length() < 1.0) {
                return p;
            }
        }
    }

    static vec3 random_unit_vector() {
        double a = random_double(0, 2 * std::numbers::pi);
        double z = random_double(-1, 1);
        double r = std::sqrt(1 - z * z);
        return {r * std::cos(a), r * std::sin(a), z};
    }

private:
    std::array<double, 3> data;
};

inline vec3 operator*(double scalar, const vec3& v) {
    return v * scalar;
}

inline double dot(const vec3& a, const vec3& b) {
    return a.x() * b.x() + a.y() * b.y() + a.z() * b.z();
}

inline vec3 cross(const vec3& a, const vec3& b) {
    return {
        a.y() * b.z() - a.z() * b.y(),
        a.z() * b.x() - a.x() * b.z(),
        a.x() * b.y() - a.y() * b.x()
    };
}

using point3 = vec3;
using color3 = vec3;


#endif //VEC3_H
