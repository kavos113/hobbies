#include <thrust/reduce.h>
#include <thrust/device_vector.h>
#include <thrust/host_vector.h>
#include <thrust/random.h>
#include <iostream>
#include <chrono>

#define ARRAY_SIZE 16384 * 16384

int main()
{
    thrust::default_random_engine rng(1337);
    thrust::uniform_int_distribution<int> dist;
    thrust::host_vector<int> h_array(ARRAY_SIZE);
    thrust::generate(h_array.begin(), h_array.end(), [&]() { return dist(rng); });

    thrust::device_vector<int> d_array = h_array;

    // warmup
    thrust::reduce(d_array.begin(), d_array.end(), 0, thrust::plus<int>());

    auto start = std::chrono::high_resolution_clock::now();
    int sum = thrust::reduce(d_array.begin(), d_array.end(), 0, thrust::plus<int>());
    auto end = std::chrono::high_resolution_clock::now();

    std::chrono::duration<double, std::milli> duration = end - start;
    std::cout << "Sum: " << sum << std::endl;
    std::cout << "Time: " << duration.count() << " ms" << std::endl;
}