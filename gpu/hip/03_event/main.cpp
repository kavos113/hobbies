#include <hip/hip_runtime.h>

#include <chrono>
#include <iostream>
#include <random>
#include <vector>

#define HIP_CHECK(expr)                                                                            \
  do                                                                                               \
  {                                                                                                \
    hipError_t err = (expr);                                                                       \
    if (err != hipSuccess)                                                                         \
    {                                                                                              \
      std::cerr << "[HIP Error]: " << #expr << " failed, " << hipGetErrorString(err) << std::endl; \
      return 1;                                                                                    \
    }                                                                                              \
  } while (0)

__global__ void
vecAdd(float *a, float *b, float *c, int n)
{
  int i = threadIdx.x + blockDim.x * blockIdx.x;
  if (i < n)
  {
    c[i] = a[i] + b[i];
  }
}

int
main(int argc, char **argv)
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << argv[0] << " <blocksize>" << std::endl;
    return 1;
  }

  float *a_d, *b_d, *c_d;
  constexpr size_t n = (1u << 22) + 13;
  constexpr size_t iter = 100;
  size_t blockSize = strtol(argv[1], nullptr, 10);

  constexpr int seed = 12345;
  std::mt19937 gen(seed);
  std::uniform_real_distribution<float> dist(0.0f, 10.0f);

  std::vector<float> a(n);
  std::vector<float> b(n);

  for (int i = 0; i < n; i++)
  {
    a[i] = dist(gen);
    b[i] = dist(gen);
  }

  size_t size = n * sizeof(float);

  HIP_CHECK(hipSetDevice(1));

  HIP_CHECK(hipMalloc((void **)&a_d, size));
  HIP_CHECK(hipMalloc((void **)&b_d, size));
  HIP_CHECK(hipMalloc((void **)&c_d, size));

  HIP_CHECK(hipMemcpy(a_d, a.data(), size, hipMemcpyHostToDevice));
  HIP_CHECK(hipMemcpy(b_d, b.data(), size, hipMemcpyHostToDevice));

  size_t gridSize = (n + blockSize - 1) / blockSize;

  // warmup
  for (int i = 0; i < 10; i++)
  {
    vecAdd<<<gridSize, blockSize>>>(a_d, b_d, c_d, n);
    HIP_CHECK(hipGetLastError());
  }

  HIP_CHECK(hipDeviceSynchronize());

  hipEvent_t start, end;
  HIP_CHECK(hipEventCreate(&start));
  HIP_CHECK(hipEventCreate(&end));

  HIP_CHECK(hipEventRecord(start));

  std::chrono::steady_clock::time_point s_cpu = std::chrono::steady_clock::now();

  for (int i = 0; i < iter; i++)
  {
    vecAdd<<<gridSize, blockSize>>>(a_d, b_d, c_d, n);
    HIP_CHECK(hipGetLastError());
  }

  HIP_CHECK(hipEventRecord(end));
  HIP_CHECK(hipEventSynchronize(end));

  std::chrono::steady_clock::time_point e_cpu = std::chrono::steady_clock::now();

  float ms;
  HIP_CHECK(hipEventElapsedTime(&ms, start, end));

  std::chrono::microseconds us_cpu = std::chrono::duration_cast<std::chrono::microseconds>(e_cpu - s_cpu);

  std::vector<float> c(n);
  HIP_CHECK(hipMemcpy(c.data(), c_d, size, hipMemcpyDeviceToHost));

  std::cout << "GPU time: " << ms << " ms" << std::endl;
  std::cout << "CPU time: " << static_cast<double>(us_cpu.count()) / 1000.0 << " ms" << std::endl;

  HIP_CHECK(hipFree(a_d));
  HIP_CHECK(hipFree(b_d));
  HIP_CHECK(hipFree(c_d));

  return 0;
}