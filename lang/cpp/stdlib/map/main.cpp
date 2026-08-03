#include <print>
#include <map>
#include <string>

int main()
{
    std::map<int, std::string> somemap = {
        {0, "aaaa"},
        {1, "bbbb"},
        {2, "dddd"}
    };

    std::println("map[1] = {}", somemap[1]);

    std::println("out of range: {}", somemap[3]);

    try
    {
        std::string bad = somemap.at(5);
    }
    catch (std::out_of_range& e)
    {
        std::println(".at() throw exception out_of_range");
    }

    return 0;
}