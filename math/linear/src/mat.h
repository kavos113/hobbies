#ifndef LINEAR_SRC_MAT_H
#define LINEAR_SRC_MAT_H

#include <vector>
#include <iostream>
#include <random>

template <typename T>
class mat
{
public:
    size_t rows, cols;
    std::vector<T> data;

    mat(size_t r, size_t c) : rows(r), cols(c), data(r * c) {}

    static mat identity(size_t n) {
        mat I(n, n);
        for (size_t i = 0; i < n; ++i) {
            I(i, i) = T(1);
        }
        return I;
    }

    static mat zeros(size_t r, size_t c) {
        return mat(r, c);
    }

    static mat random(size_t r, size_t c) {
        mat m(r, c);
        std::random_device rd;
        std::mt19937 gen(rd());
        std::uniform_real_distribution<T> dist(0, 1);
        for (size_t i = 0; i < r; ++i) {
            for (size_t j = 0; j < c; ++j) {
                m(i, j) = dist(gen);
            }
        }
        return m;
    }

    T& operator()(size_t i, size_t j) {
        return data[i * cols + j];
    }

    const T& operator()(size_t i, size_t j) const {
        return data[i * cols + j];
    }

    mat operator+(const mat& other) const {
        mat result(rows, cols);
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < cols; ++j) {
                result(i, j) = (*this)(i, j) + other(i, j);
            }
        }
        return result;
    }

    mat operator-(const mat& other) const {
        mat result(rows, cols);
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < cols; ++j) {
                result(i, j) = (*this)(i, j) - other(i, j);
            }
        }
        return result;
    }

    mat operator*(const mat& other) const {
        mat result(rows, other.cols);
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < other.cols; ++j) {
                T sum = T();
                for (size_t k = 0; k < cols; ++k) {
                    sum += (*this)(i, k) * other(k, j);
                }
                result(i, j) = sum;
            }
        }
        return result;
    }

    mat operator*(const T& k) const {
        mat result(rows, cols);
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < cols; ++j) {
                result(i, j) = (*this)(i, j) * k;
            }
        }
        return result;
    }

    mat operator/(const T& k) const {
        mat result(rows, cols);
        for (size_t i = 0; i < rows; ++i) {
            for (size_t j = 0; j < cols; ++j) {
                result(i, j) = (*this)(i, j) / k;
            }
        }
        return result;
    }
};

template <typename CharT, typename Traits, typename T>
std::basic_ostream<CharT, Traits>&
operator<<(std::basic_ostream<CharT, Traits>& os, const mat<T>& m) {
    for (size_t i = 0; i < m.rows; ++i) {
        for (size_t j = 0; j < m.cols; ++j) {
            os << m(i, j) << (j < m.cols - 1 ? " " : "");
        }
        os << "\n";
    }
    return os;
}

#endif //LINEAR_SRC_MAT_H