#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define SIZE 1024

int main(int argc, char **argv)
{
  srand(time(NULL));

  time_t start, end;

  int *matA = (int *)malloc(SIZE * SIZE * sizeof(int));
  int *matB = (int *)malloc(SIZE * SIZE * sizeof(int));
  int *matC = (int *)malloc(SIZE * SIZE * sizeof(int));

  for (int i = 0; i < SIZE; i++)
  {
    for (int j = 0; j < SIZE; j++)
    {
      matA[i * SIZE + j] = rand() % (1024 * 1024);
      matB[i * SIZE + j] = rand() % (1024 * 1024);
      matC[i * SIZE + j] = 0;
    }
  }

  // mul
  start = clock();
  for (int i = 0; i < SIZE; i++)
  {
    for (int j = 0; j < SIZE; j++)
    {
      for (int k = 0; k < SIZE; k++)
      {
        matC[i * SIZE + j] += matA[i * SIZE + k] * matB[k * SIZE + j];
      }
    }
  }
  end = clock();
  printf("Time taken: %f ms\n", (double)(end - start) / CLOCKS_PER_SEC * 1000);

  free(matA);
  free(matB);
  free(matC);

  return 0;
}

// 4447.06 ms