#include <iostream>

class SomeClass
{
public:
    // this is lazy initializing
    SomeClass()
    {
        std::cout << "some class constructor called" << std::endl;
        val = 3;
    }

    int val = 0;
};

int someFunc(bool flag)
{
    if (!flag)
    {
        return -1;
    }

    static SomeClass some;
    return some.val;
}

int main()
{
    std::cout << "result of somefunc with false: " << someFunc(false) << std::endl;
    std::cout << "result of somefunc with true : " << someFunc(true) << std::endl;
}