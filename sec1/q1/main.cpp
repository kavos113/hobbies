#include <iostream>

int main()
{
    unsigned int max;

    std::cout << "Enter the maximum number: ";
    std::cin >> max;

    unsigned long long total = 0;
    for (int i = 1; i <= max; i++)
    {
        if (i % 3 == 0 || i % 5 == 0)
        {
            total += i;
        }
    }

    std::cout << "Result: " << total << std::endl;
}