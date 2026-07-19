#include <iostream>
#include <filesystem>
#include <string>

#include "App.h"
#include "shaders/00_entries.h"

static void usage()
{
    std::cout << "usage: ./sdf <entry index (by 0)>" << std::endl;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        usage();
        return 1;
    }

    int index = std::stoi(argv[1]);
    if (index < 0 || index >= ENTRIES.size())
    {
        std::cerr << "unknown index: " << index << std::endl;
    }

    namespace fs = std::filesystem;

    if (!fs::exists(ENTRIES[index].shaderFile))
    {
        std::cerr << "File not found: " << ENTRIES[index].shaderFile << std::endl;
        return 1;
    }

    App app(index);

    app.run();

    return 0;
}