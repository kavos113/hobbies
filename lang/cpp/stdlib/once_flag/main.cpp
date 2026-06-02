#include <iostream>
#include <mutex>

class SomeClass
{
public:
    void initialize()
    {
        std::cout << "Calling initialize()" << std::endl;

        std::call_once(initFlag, []() {
            std::cout << "Initializing..." << std::endl;
        });
    }
private:
    static std::once_flag initFlag;
};

std::once_flag SomeClass::initFlag;

int main()
{
    SomeClass obj1;
    SomeClass obj2;

    obj1.initialize();
    obj2.initialize();

    return 0;
}