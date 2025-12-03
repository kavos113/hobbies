#include <iostream>

int gcd(unsigned int a, unsigned int b)
{
    if (b == 0) return a;
    return gcd(b, a % b);
}

int main()
{
    unsigned int a, b;
    std::cout << "Enter a, b: ";
    std::cin >> a >> b;

    std::cout << "gcd is: " << gcd(a, b) << std::endl;
}