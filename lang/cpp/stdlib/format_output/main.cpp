#include <iostream>

int main()
{
    char c = 0x41;

    std::cout << c << "num: " << static_cast<int>(c) << "\thex output; " << std::hex << static_cast<int>(c) << std::endl;

    return 0;
}