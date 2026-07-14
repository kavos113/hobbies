newで確保したものを用いてスマートポインタを作ると，所有権がスマートポインタに移る。
そのため，そのスマートポインタの参照カウントが0になると，deleteを明示的に呼ばなくても解放される。

```c++
auto *ptr = new SomeStruct(1, 2);

{
    auto smart = std::unique_ptr<SomeStruct>(ptr);
} // ここで解放される
```