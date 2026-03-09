# 配列の各要素の和

16384 * 16384サイズ

### 単純な実装

| block size | time (ms) |
| ---------- | --------- |
| 32         | 9.267     |
| 64         | 5.027     |
| 256        | 5.193     |
| 512        | 5.085     |
| 1024       | 5.040     |

32とそれ以外で差があるのはwarp schedulingをしているから？

### トーナメント方式
これ以降はblock sizeは256で固定

time: 7.034 ms

### 少しだけ改善
```cpp
// before
for (unsigned int s = 1; s < blockDim.x; s *= 2)
{
  int index = 2 * s * tid;
  if (index < blockDim.x)
  {
    sdata[index] += sdata[index + s]; 
  }
  __syncthreads();
}

// after
for (unsigned int s = blockDim.x / 2; s > 0; s >>= 1)
{
  if (tid < s)
  {
    sdata[tid] += sdata[tid + s];
  }
  __syncthreads();
}
```

time: 6.805 ms

### double load
2つだけロードしてきて、足し合わせてから共有メモリに保存する
```cpp
//before
unsigned int idx = blockIdx.x * blockDim.x + threadIdx.x;
sdata[tid] = arrayI[idx];

//after
unsigned int idx = blockIdx.x * (blockDim.x * 2) + threadIdx.x;
sdata[tid] = arrayI[idx] + arrayI[idx + blockDim.x];
```

time: 3.649 ms

4倍にしても特に変わらなかった

### さらに改善
sが32以下になったら、同一warp内での計算になるので、__syncthreads()は不要になる
```cpp 
if (tid < 32)
{
  volatile int *vsmem = sdata;
  vsmem[tid] += vsmem[tid + 32];
  vsmem[tid] += vsmem[tid + 16];
  vsmem[tid] += vsmem[tid + 8];
  vsmem[tid] += vsmem[tid + 4];
  vsmem[tid] += vsmem[tid + 2];
  vsmem[tid] += vsmem[tid + 1];
}
```

time: 3.070 ms