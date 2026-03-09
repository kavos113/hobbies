#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define MAT_SIZE 4096
#define BLOCK_SIZE 16

__global__ void matmul_shared(int *matA, int *matB, int *matC)
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

__global__ void matmul(int *matA, int *matB, int *matC)
{
  int row = blockIdx.y * blockDim.y + threadIdx.y;
  int col = blockIdx.x * blockDim.x + threadIdx.x;

  int sum = 0;
  for (int k = 0; k < MAT_SIZE; k++)
  {
    sum += matA[row * MAT_SIZE + k] * matB[k * MAT_SIZE + col];
  }
  matC[row * MAT_SIZE + col] = sum;
}

void matmul_cpu(int *matA, int *matB, int *matC)
{
  for (int i = 0; i < MAT_SIZE; i++)
  {
    for (int j = 0; j < MAT_SIZE; j++)
    {
      int sum = 0;
      for (int k = 0; k < MAT_SIZE; k++)
      {
        sum += matA[i * MAT_SIZE + k] * matB[k * MAT_SIZE + j];
      }
      matC[i * MAT_SIZE + j] = sum;
    }
  }
}

int *h_matA;
int *h_matB;
int *h_matC;

int *d_matA;
int *d_matB;
int *d_matC;

void init_matrices()
{
  int matrix_size = MAT_SIZE * MAT_SIZE * sizeof(int);

  h_matA = (int *)malloc(matrix_size);
  h_matB = (int *)malloc(matrix_size);
  h_matC = (int *)malloc(matrix_size);

  for (int i = 0; i < MAT_SIZE; i++)
  {
    for (int j = 0; j < MAT_SIZE; j++)
    {
      h_matA[i * MAT_SIZE + j] = rand() % (1024 * 1024);
      h_matB[i * MAT_SIZE + j] = rand() % (1024 * 1024);
    }
  }

  cudaMalloc((void **)&d_matA, matrix_size);
  cudaMalloc((void **)&d_matB, matrix_size);
  cudaMalloc((void **)&d_matC, matrix_size);
  cudaMemcpy(d_matA, h_matA, matrix_size, cudaMemcpyHostToDevice);
  cudaMemcpy(d_matB, h_matB, matrix_size, cudaMemcpyHostToDevice);
}

void cleanup()
{
  free(h_matA);
  free(h_matB);
  free(h_matC);
  cudaFree(d_matA);
  cudaFree(d_matB);
  cudaFree(d_matC);
}

int main(int argc, char **argv)
{
  if (argc < 2)
  {
    printf("Usage: %s <use_cpu>\n", argv[0]);
    return 1;
  }

  int use_cpu = atoi(argv[1]);

  srand(time(NULL));

  time_t start, end;

  int matrix_size = MAT_SIZE * MAT_SIZE * sizeof(int);

  // --------------- CPU ---------------
  if (use_cpu)
  {
    init_matrices();

    start = clock();
    matmul_cpu(h_matA, h_matB, h_matC);
    end = clock();
    printf("CPU Time taken: %f ms\n", (double)(end - start) / CLOCKS_PER_SEC * 1000);

    cleanup();
  }

  // --------------- GPU ---------------
  dim3 block(BLOCK_SIZE, BLOCK_SIZE);
  dim3 grid(MAT_SIZE / block.x, MAT_SIZE / block.y);

  // 1. matrix mul
  init_matrices();

  start = clock();
  matmul<<<grid, block>>>(d_matA, d_matB, d_matC);
  cudaDeviceSynchronize();
  end = clock();

  cudaMemcpy(h_matC, d_matC, matrix_size, cudaMemcpyDeviceToHost);
  printf("GPU Time taken: %f ms\n", (double)(end - start) / CLOCKS_PER_SEC * 1000);
  cleanup();

  // 2. matrix mul with shared memory
  init_matrices();

  start = clock();
  matmul_shared<<<grid, block>>>(d_matA, d_matB, d_matC);
  cudaDeviceSynchronize();
  end = clock();

  cudaMemcpy(h_matC, d_matC, matrix_size, cudaMemcpyDeviceToHost);
  printf("GPU Shared Memory Time taken: %f ms\n", (double)(end - start) / CLOCKS_PER_SEC * 1000);
  cleanup();

  return 0;
}
