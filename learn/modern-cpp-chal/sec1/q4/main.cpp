#include <iostream>

bool is_prime(unsigned int x)
{
    for (int i = 2; i * i <= x; i++)
    {
        if (x % i == 0)
        {
            return false;
        }
    }

    return true;
}

int main()
{
    unsigned int max;
    std::cout << "Enter a number: ";
    std::cin >> max;

    for (unsigned int i = max; i > 0; i--)
    {
        if (is_prime(i))
        {
            std::cout << "max: " << i << std::endl;
            break;
        }
    }
}