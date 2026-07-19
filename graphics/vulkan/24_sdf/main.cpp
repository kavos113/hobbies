#include <iostream>
#include <filesystem>
#include <string>

#include "App.h"

void usage()
{
    std::cout << "usage: ./sdf <fragment shader path>" << std::endl;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        usage();
        return 1;
    }

    namespace fs = std::filesystem;

    std::string fsFile(argv[1]);
    if (!fs::exists(fsFile))
    {
        std::cerr << "File not found: " << fsFile << std::endl;
        return 1;
    }

    App app(fsFile);

    app.run();

    return 0;
}