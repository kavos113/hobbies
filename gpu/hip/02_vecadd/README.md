# vecadd

カーネル
```cpp
__global__ void
vecAdd(float *a, float *b, float *c, int n)
{
  int i = threadIdx.x + blockDim.x * blockIdx.x;
  if (i < n)
  {
    c[i] = a[i] + b[i];
  }
}
```

ホストコード
```cpp
hipMalloc(&a_d, size);
hipMalloc(&b_d, size);
hipMalloc(&c_d, size);

hipMemcpy(a_d, a.data(), size, hipMemcpyHostToDevice);
hipMemcpy(b_d, b.data(), size, hipMemcpyHostToDevice);

vecAdd<<<std::ceil(n / 256.0), 256>>>(a_d, b_d, c_d, n);

hipMemcpy(c.data(), c_d, size, hipMemcpyDeviceToHost);

hipFree(a_d);
hipFree(b_d);
hipFree(c_d);
```