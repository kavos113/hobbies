#ifndef MOD_H
#define MOD_H

#include <vector>

inline bool is_mod(int n, int div, const std::vector<int>& mod_values)
{
    for (int mod : mod_values)
    {
        if (n % div == mod)
        {
            return true;
        }
    }
    return false;
}

#endif // MOD_H