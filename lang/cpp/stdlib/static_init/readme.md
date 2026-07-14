ローカルstatic変数の初期化は遅延初期化

```c++
int someFunc(bool flag)
{
    if (!flag)
    {
        return -1; // このときはsomeは初期化されない
    }

    static SomeClass some;
    return some.val;
}
```