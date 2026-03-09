#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define ARRAY_SIZE 16384 * 16384
#define BLOCK_SIZE 256

__global__ void asum(int *arrayI, int *out)
{
  __shared__ int sdata[BLOCK_SIZE];

  unsigned int tid = threadIdx.x;
  unsigned int idx = blockIdx.x * blockDim.x + threadIdx.x;

  sdata[tid] = arrayI[idx];
  __syncthreads();

  // s = 1: sdata[0] += sdata[1], sdata[2] += sdata[3], ...
  // s = 2: sdata[0] += sdata[2], sdata[4] += sdata[6], ...
  // s = 4: sdata[0] += sdata[4], sdata[8] += sdata[12], ...
  // ...
  // s = 128: sdata[0] += sdata[128] -> RESULT in sdata[0]
  for (unsigned int s = 1; s < blockDim.x; s *= 2)
  {
    // 足される側のindex
    int index = 2 * s * tid;
    if (index < blockDim.x)
    {
      sdata[index] += sdata[index + s];
    }
    __syncthreads();
  }

  if (tid == 0)
  {
    atomicAdd(out, sdata[0]);
  }
}

__global__ void init_array(int *array)
{
  int idx = blockIdx.x * blockDim.x + threadIdx.x;
  array[idx] = (idx * 41239898349) % 1000;
}

int *h_arrayI, *h_out;
int *d_arrayI, *d_out;

void init_array(int block_size)
{
  dim3 grid((ARRAY_SIZE + block_size - 1) / block_size);
  dim3 block(block_size);
  init_array<<<grid, block>>>(d_arrayI);

  *h_out = 0;
  cudaMemcpy(d_out, h_out, sizeof(int), cudaMemcpyHostToDevice);
}

int main(int argc, char *argv[])
{
  if (argc < 2)
  {
    printf("Usage: %s <iterations>\n", argv[0]);
    return 1;
  }

  int iterations = atoi(argv[1]);

  srand(time(NULL));

  time_t start, end;

  unsigned int array_size = ARRAY_SIZE * sizeof(int);

  h_arrayI = (int *)malloc(array_size);
  h_out = (int *)malloc(sizeof(int));

  cudaMalloc((void **)&d_arrayI, array_size);
  cudaMalloc((void **)&d_out, sizeof(int));

  // warmup
  init_array(BLOCK_SIZE);
  dim3 block(BLOCK_SIZE);
  dim3 grid((ARRAY_SIZE + block.x - 1) / block.x);
  asum<<<grid, block>>>(d_arrayI, d_out);
  cudaDeviceSynchronize();
  cudaMemcpy(h_out, d_out, sizeof(int), cudaMemcpyDeviceToHost);

  double total_time = 0;
  for (int i = 0; i < iterations; i++)
  {
    init_array(BLOCK_SIZE);

    dim3 block(BLOCK_SIZE);
    dim3 grid((ARRAY_SIZE + block.x - 1) / block.x);

    start = clock();
    asum<<<grid, block>>>(d_arrayI, d_out);
    cudaDeviceSynchronize();
    end = clock();

    cudaMemcpy(h_out, d_out, sizeof(int), cudaMemcpyDeviceToHost);
    printf("GPU Time taken: %f ms\n", (double)(end - start) / CLOCKS_PER_SEC * 1000);

    total_time += (double)(end - start) / CLOCKS_PER_SEC * 1000;
  }

  printf("Average GPU Time taken: %f ms\n", total_time / iterations);

  free(h_arrayI);
  free(h_out);
  cudaFree(d_arrayI);
  cudaFree(d_out);

  return 0;
}