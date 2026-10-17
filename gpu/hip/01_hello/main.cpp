#include <hip/hip_runtime.h>
#include <stdio.h>

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
gpuHelloWorld()
{
  int id = blockIdx.x * blockDim.x + threadIdx.x;
  printf("hello world from block %d, thread %d\n", blockIdx.x, id);
}

int
main()
{
  printf("launching gpu kernel...\n");

  int count;
  HIP_CHECK(hipGetDeviceCount(&count));
  printf("[HIP] Device count: %d\n", count);

  for (int i = 0; i < count; i++)
  {
    hipDeviceProp_t prop;
    HIP_CHECK(hipGetDeviceProperties(&prop, i));

    printf("Device name:   %s\n", prop.name);
    printf("Architecture:  %s\n", prop.gcnArchName);
    printf("Global Memory: %zu\n", prop.totalGlobalMem);
    printf("Warp Size:     %d\n", prop.warpSize);
    printf("------------------------\n");
  }

  HIP_CHECK(hipSetDevice(1));

  int current = -1;
  hipGetDevice(&current);
  printf("[HIP]: current device: %d\n", current);

  gpuHelloWorld<<<8, 8>>>();

  HIP_CHECK(hipGetLastError());
  HIP_CHECK(hipDeviceSynchronize());

  return 0;
}