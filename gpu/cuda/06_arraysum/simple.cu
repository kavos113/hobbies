#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define ARRAY_SIZE 16384 * 16384

__global__ void asum_simple(int *arrayI, int *arrayO)
{
  int idx = blockIdx.x * blockDim.x + threadIdx.x;

  atomicAdd(arrayO, arrayI[idx]);
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
  if (argc < 3)
  {
    printf("Usage: %s <iterations> <block_size>\n", argv[0]);
    return 1;
  }

  int iterations = atoi(argv[1]);
  int block_size = atoi(argv[2]);

  srand(time(NULL));

  time_t start, end;

  unsigned int array_size = ARRAY_SIZE * sizeof(int);

  h_arrayI = (int *)malloc(array_size);
  h_out = (int *)malloc(sizeof(int));

  cudaMalloc((void **)&d_arrayI, array_size);
  cudaMalloc((void **)&d_out, sizeof(int));

  // warmup
  init_array(block_size);
  dim3 block(block_size);
  dim3 grid((ARRAY_SIZE + block.x - 1) / block.x);
  asum_simple<<<grid, block>>>(d_arrayI, d_out);
  cudaDeviceSynchronize();
  cudaMemcpy(h_out, d_out, sizeof(int), cudaMemcpyDeviceToHost);

  double total_time = 0;
  for (int i = 0; i < iterations; i++)
  {
    init_array(block_size);

    dim3 block(block_size);
    dim3 grid((ARRAY_SIZE + block.x - 1) / block.x);

    start = clock();
    asum_simple<<<grid, block>>>(d_arrayI, d_out);
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