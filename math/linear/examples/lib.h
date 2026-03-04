#ifndef LINEAR_EXAMPLES_LIB_H
#define LINEAR_EXAMPLES_LIB_H

#include <iostream>
#include <string>

template <typename T>
void assert_equal(const T& a, const T& b, const std::string& message = "") {
    if (a != b) {
        std::cerr << "Assertion failed: " << a << " != " << b;
        if (!message.empty()) {
            std::cerr << " (" << message << ")";
        }
        std::cerr << std::endl;
        std::abort();
    }
}

inline void assert_double_equal(double a, double b, const std::string& message = "") {
    double tol = 1e-9;
    if (std::abs(a - b) > tol) {
        std::cerr << "Assertion failed: " << a << " != " << b;
        if (!message.empty()) {
            std::cerr << " (" << message << ")";
        }
        std::cerr << std::endl;
        std::abort();
    }
}

#endif //LINEAR_EXAMPLES_LIB_H