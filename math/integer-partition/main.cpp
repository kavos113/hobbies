#include <iostream>
#include <vector>
#include <string>

#include "partition.h"
#include "mod.h"

void usage()
{
    std::cout << "Usage: ./partition <n> [option]" << std::endl;
    std::cout << "Options:" << std::endl;
    std::cout << "  -p: Print all partitions of n" << std::endl;
}

// l_{i} - l_{i+int} >= threshold: true
bool is_rr(const std::vector<int>& partition, int interval, int threshold)
{
    for (int i = 0; i < static_cast<int>(partition.size()) - interval; ++i)
    {
        if (partition[i] - partition[i + interval] < threshold)
        {
            return false;
        }
    }
    return true;
}

bool all_mod(const std::vector<int>& partition, int div, const std::vector<int>& mod_values)
{
    for (int value : partition)
    {
        if (!is_mod(value, div, mod_values))
        {
            return false;
        }
    }
    return true;
}

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        usage();
        return 1;
    }

    bool print_all = false;
    if (argc == 3 && std::string(argv[2]) == "-p")
    {
        print_all = true;
    }

    int n = std::stoi(argv[1]);
    if (n <= 0)
    {
        usage();
        return 1;
    }

    std::vector<std::vector<int>> partitions = partition(n);

    std::cout << "Total partitions of " << n << ": " << partitions.size() << std::endl;

    int rr = 0, mod = 0;
    for (const auto& p : partitions)
    {
        if (is_rr(p, 2, 2))
        {
            if (print_all) print_partition(p);
            rr++;
        }
    }
    if (print_all) std::cout << "------------------------" << std::endl;
    for (const auto& p : partitions)
    {
        if (all_mod(p, 7, {1, 2, 5, 6}))
        {
            if (print_all) print_partition(p);
            mod++;
        }
    }

    if (print_all) std::cout << "------------------------" << std::endl;

    std::cout << "rr : " << rr << std::endl;
    std::cout << "mod: " << mod << std::endl;

    return 0;
}