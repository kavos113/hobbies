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
```

実行結果例

| block size | CPU Time | GPU Time |
| ---------- | -------- | -------- |
| 32         | 6.676 ms | 6.266 ms |
| 64         | 5.680 ms | 5.269 ms |
| 128        | 5.138 ms | 4.643 ms |
| 256        | 4.744 ms | 4.337 ms | 