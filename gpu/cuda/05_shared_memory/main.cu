#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define MAT_SIZE 1024
#define BLOCK_SIZE 16

__global__ void matrix_mul_kernel(int *matA, int *matB, int *matC)
{
  __shared__ int tileA[BLOCK_SIZE][BLOCK_SIZE];
  __shared__ int tileB[BLOCK_SIZE][BLOCK_SIZE];

  int row = blockIdx.y * blockDim.y + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;
  int sum = 0;

  for (int i = 0; i < MAT_SIZE / BLOCK_SIZE; i++)
  {
    // タイルごとに共有メモリにコピーする
    tileA[threadIdx.y][threadIdx.x] = matA[row * MAT_SIZE + (i * BLOCK_SIZE + threadIdx.x)];
    tileB[threadIdx.y][threadIdx.x] = matB[(i * BLOCK_SIZE + threadIdx.y) * MAT_SIZE + col];

    __syncthreads();

    for (int j = 0; j < BLOCK_SIZE; j++)
    {
      sum += tileA[threadIdx.y][j] * tileB[j][threadIdx.x];
    }

    __syncthreads();
  }

  matC[row * MAT_SIZE + col] = sum;
}

int main(int argc, char **argv)
{
  srand(time(NULL));

  time_t start, end;

  unsigned int matrix_size = MAT_SIZE * MAT_SIZE * sizeof(int);

  int *h_matA = (int *)malloc(matrix_size);
  int *h_matB = (int *)malloc(matrix_size);
  int *h_matC = (int *)malloc(matrix_size);

  for (int i = 0; i < MAT_SIZE; i++)
  {
    for (int j = 0; j < MAT_SIZE; j++)
    {
      h_matA[i * MAT_SIZE + j] = rand() % (1024 * 1024);
      h_matB[i * MAT_SIZE + j] = rand() % (1024 * 1024);
    }
  }

  int *d_matA, *d_matB, *d_matC;
  cudaMalloc((void **)&d_matA, matrix_size);
  cudaMalloc((void **)&d_matB, matrix_size);
  cudaMalloc((void **)&d_matC, matrix_size);
  cudaMemcpy(d_matA, h_matA, matrix_size, cudaMemcpyHostToDevice);
  cudaMemcpy(d_matB, h_matB, matrix_size, cudaMemcpyHostToDevice);

  dim3 block(BLOCK_SIZE, BLOCK_SIZE);
  dim3 grid(MAT_SIZE / block.x, MAT_SIZE / block.y);

  start = clock();
  matrix_mul_kernel<<<grid, block>>>(d_matA, d_matB, d_matC);
  cudaDeviceSynchronize();
  end = clock();

  cudaMemcpy(h_matC, d_matC, matrix_size, cudaMemcpyDeviceToHost);

  free(h_matA);
  free(h_matB);
  free(h_matC);
  cudaFree(d_matA);
  cudaFree(d_matB);
  cudaFree(d_matC);

  printf("Time taken: %f ms\n", (double)(end - start) / CLOCKS_PER_SEC * 1000);

  return 0;
}

// 1.450 ms