#include <stdio.h>

__global__ void hello_cuda() {
    printf("Hello from CUDA kernel! Block %d, Thread %d\n", blockIdx.x, threadIdx.x);
}

int main() {
    printf("Hello from host code!\n");
    hello_cuda<<<1, 5>>>();
    cudaDeviceSynchronize();
    return 0;
}