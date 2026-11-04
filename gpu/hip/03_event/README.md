# event

GPU上でイベント計測

```cpp
hipEvent_t start, end;
hipEventCreate(&start);
hipEventCreate(&end);

hipEventRecord(start);

kernelFunc<<<256, 256>>>();

hipEventRecord(end);
hipEventSynchronize(end);

float ms;
hipEventElapsedTime(&ms, start, end);

hipEventDestroy(start);
hipEventDestroy(end);
```

実行結果例 (vecadd * 100回)

| block size | CPU Time | GPU Time |
| ---------- | -------- | -------- |
| 32         | 0.070 ms | 6.162 ms |
| 64         | 0.072 ms | 4.487 ms |
| 128        | 0.080 ms | 4.589 ms |
| 256        | 0.070 ms | 4.680 ms | 