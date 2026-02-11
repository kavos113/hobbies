# 07_wm_timer

[MSDN](https://learn.microsoft.com/ja-jp/windows/win32/winmsg/using-timers)

タイマーの設定
```c++
#define CUSTOM_TIMER_ID 1

SetTimer(
    m_hwnd,
    CUSTOM_TIMER_ID,
    1000,
    nullptr // コールバック関数
);
```

ハンドラでの処理
```c++
case WM_TIMER:
    if (wParam == CUSTOM_TIMER_ID) {
        // タイマーイベントの処理
    }
    break;
```

コールバック関数の使用
```c++
void CALLBACK TimerProc(
    HWND hwnd,
    UINT uMsg,
    UINT_PTR idEvent, // ここには CUSTOM_TIMER_ID が入る
    DWORD dwTime // system time
) {
    
}

SetTimer(
    m_hwnd,
    CUSTOM_TIMER_ID,
    1000,
    TimerProc 
);
```