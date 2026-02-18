#include <stdio.h>
#include <string.h>

__global__ void hello_cuda() 
{
    printf("Hello from CUDA kernel! Block %d, Thread %d\n", blockIdx.x, threadIdx.x);
}

int main(int argc, char *argv[])
{
    if (argc == 2 && strcmp(argv[1], "s") == 0) 
    {
        printf("Using streams\n");

        cudaStream_t streamA;
        cudaStream_t streamB;
        cudaStreamCreate(&streamA);
        cudaStreamCreate(&streamB);

        hello_cuda<<<1, 32, 0, streamA>>>();
        hello_cuda<<<1, 32, 0, streamB>>>();

        cudaStreamSynchronize(streamA);
        cudaStreamSynchronize(streamB);

        cudaStreamDestroy(streamA);
        cudaStreamDestroy(streamB);
    }
    else 
    {
        printf("Not using streams\n");

        hello_cuda<<<1, 32>>>();
        hello_cuda<<<1, 32>>>();

        cudaDeviceSynchronize();
    }

    return 0;
}