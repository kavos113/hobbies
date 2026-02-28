#include "../src/mat.h"
#include <iostream>

int main() {
    mat<double> A(2, 3);
    A(0, 0) = 1; A(0, 1) = 2; A(0, 2) = 3;
    A(1, 0) = 4; A(1, 1) = 5; A(1, 2) = 6;

    mat<double> B(3, 2);
    B(0, 0) = 7; B(0, 1) = 8;
    B(1, 0) = 9; B(1, 1) = 10;
    B(2, 0) = 11; B(2, 1) = 12;

    mat<double> C = A * B;

    std::cout << "Result of A * B:" << std::endl;
    std::cout << C << std::endl;

    return 0;
}