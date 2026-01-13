#include <iostream>
#include <memory>

struct SomeStruct
{
    int a;
    int b;
};

int main()
{
    auto *ptr = new SomeStruct(1, 2);
    std::cout << "ptr from new: " << ptr << " (" << ptr->a << ", " << ptr->b << ")" << std::endl;

    {
        auto smart = std::unique_ptr<SomeStruct>(ptr);
        std::cout << "smart ptr: " << smart << " (" << smart->a << ", " << smart->b << ")" << std::endl;
    }

    // already freed
    std::cout << "ptr after smart scope: " << ptr << " (" << ptr->a << ", " << ptr->b << ")" << std::endl;
    // delete ptr; // abort
}