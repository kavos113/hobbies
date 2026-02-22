#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <iostream>
#include <string>

BOOL CALLBACK EnumWindowsProc(HWND hwnd, LPARAM lParam)
{
    std::wstring title(256, L'\0');
    std::wstring class_name(256, L'\0');

    bool visible = IsWindowVisible(hwnd) != 0;
    GetWindowText(hwnd, title.data(), static_cast<int>(title.size()));
    GetClassName(hwnd, class_name.data(), static_cast<int>(class_name.size()));

    std::wcout << L"HWND: " << hwnd
               << L", Visible: " << (visible ? L"Yes" : L"No")
               << L", Title: " << title.c_str()
               << L", Class: " << class_name.c_str() << std::endl;

    return TRUE;
}

int main()
{
    EnumWindows(EnumWindowsProc, 0);
    return 0;
}