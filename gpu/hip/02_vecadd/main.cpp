#include <hip/hip_runtime.h>
#include <stdio.h>

#include <random>
#include <vector>

#define HIP_CHECK(expr)                                                               \
  do                                                                                  \
  {                                                                                   \
    hipError_t err = (expr);                                                          \
    if (err != hipSuccess)                                                            \
    {                                                                                 \
      fprintf(stderr, "[HIP Error]: %s failed, %s\n", #expr, hipGetErrorString(err)); \
      return 1;                                                                       \
    }                                                                                 \
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

void
usage(char *prog)
{
  printf("usage: %s <size>\n", prog);
}

int
main(int argc, char **argv)
{
  if (argc < 2)
  {
    usage(argv[0]);
    return 1;
  }

  float *a_d, *b_d, *c_d;
  long n = strtol(argv[1], nullptr, 10);

  std::random_device rd;
  std::mt19937 gen(rd());
  std::uniform_real_distribution<float> dist(0.0f, 10.0f);

  std::vector<float> a(n);
  std::vector<float> b(n);

  for (int i = 0; i < n; i++)
  {
    a[i] = dist(gen);
    b[i] = dist(gen);
  }

  int size = n * sizeof(float);

  HIP_CHECK(hipSetDevice(1));

  HIP_CHECK(hipMalloc((void **)&a_d, size));
  HIP_CHECK(hipMalloc((void **)&b_d, size));
  HIP_CHECK(hipMalloc((void **)&c_d, size));

  HIP_CHECK(hipMemcpy(a_d, a.data(), size, hipMemcpyHostToDevice));
  HIP_CHECK(hipMemcpy(b_d, b.data(), size, hipMemcpyHostToDevice));

  vecAdd<<<std::ceil(n / 256.0), 256>>>(a_d, b_d, c_d, n);

  HIP_CHECK(hipGetLastError());

  std::vector<float> c(n);
  HIP_CHECK(hipMemcpy(c.data(), c_d, size, hipMemcpyDeviceToHost));

  for (int i = 0; i < 10; i++)
  {
    printf("%f + %f = %f\n", a[i], b[i], c[i]);
  }
  printf("...\n");

  HIP_CHECK(hipFree(a_d));
  HIP_CHECK(hipFree(b_d));
  HIP_CHECK(hipFree(c_d));

  return 0;
}