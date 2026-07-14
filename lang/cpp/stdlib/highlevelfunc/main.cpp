#include <print>
#include <ranges>
#include <vector>

int main()
{
    std::vector nums = {1, 2, 3, 4, 5, 6, 7, 8};

    std::vector<int> revSq
        = nums 
            | std::views::transform([](int x){ return x * x; })
            | std::views::reverse
            | std::ranges::to<std::vector<int>>();

    std::println("{}", revSq);
    return 0;
}