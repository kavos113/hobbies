# 01_hello

実行環境

```
Device name:   AMD Radeon(TM) Graphics
Architecture:  gfx1036
Global Memory: 62326734848
Warp Size:     32
------------------------
Device name:   AMD Radeon RX 7900 XT
Architecture:  gfx1100
Global Memory: 21458059264
Warp Size:     32
```

コンパイル: これら2つへ向けて
```
hipcc --offload-arch=gfx1100 --offload-arch=gfx1036 main.cpp -o main.exe
```
