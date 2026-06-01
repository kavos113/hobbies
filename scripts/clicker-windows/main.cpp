#include <iostream>
#include <chrono>
#include <thread>
#include <windows.h>

void usage()
{
    std::cout << "Usage: " << GetCommandLineA() << " duration(ms)" << std::endl;
}

void send()
{
    POINT cursorPos;
    GetCursorPos(&cursorPos);

    LPARAM lparam = MAKELPARAM(cursorPos.x, cursorPos.y);
    SendMessage(HWND_BROADCAST, WM_LBUTTONDOWN, MK_LBUTTON, lparam);
    std::this_thread::sleep_for(std::chrono::milliseconds(50));
    SendMessage(HWND_BROADCAST, WM_LBUTTONUP, 0, lparam);
}

int main(int argc, char **argv)
{
    if (argc != 2) {
        usage();
        return 1;
    }

    int duration = strtol(argv[1], nullptr, 10);
    if (duration <= 0) {
        usage();
        return 1;
    }

    while (true)
    {
        if (GetAsyncKeyState(VK_ESCAPE) & 0x8000) {
            break;
        }

        std::this_thread::sleep_for(std::chrono::milliseconds(duration));
        send();
    }

    return 0;
}