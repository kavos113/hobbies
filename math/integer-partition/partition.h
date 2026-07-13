#ifndef PARTITION_H
#define PARTITION_H

#include <vector>
#include <iostream>

// n: target - sum(current)
inline void recursive_partition(int n, int max, std::vector<int>& current, std::vector<std::vector<int>>& partitions)
{
    if (n == 0)
    {
        partitions.push_back(current);
        return;
    }

    for (int i = std::min(n, max); i >= 1; --i)
    {
        current.push_back(i);
        recursive_partition(n - i, i, current, partitions);
        current.pop_back();
    }
}

inline std::vector<std::vector<int>> partition(int n)
{
    std::vector<std::vector<int>> partitions;
    std::vector<int> current;
    recursive_partition(n, n, current, partitions);

    return partitions;
}

inline void print_partition(const std::vector<int>& partition)
{
    for (size_t i = 0; i < partition.size(); ++i)
    {
        std::cout << partition[i];
        if (i != partition.size() - 1)
        {
            std::cout << " ";
        }
    }
    std::cout << std::endl;
}

#endif // PARTITION_H