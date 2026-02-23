#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <tlhelp32.h>
#include <iostream>

int main()
{
    HANDLE moduleSnap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, 0);
    if (moduleSnap == INVALID_HANDLE_VALUE)
    {
        std::cerr << "Failed to create module snapshot: " << GetLastError() << std::endl;
        return 1;
    }

    MODULEENTRY32 me;
    me.dwSize = sizeof(MODULEENTRY32);

    if (!Module32First(moduleSnap, &me))
    {
        std::cerr << "Failed to get first module: " << GetLastError() << std::endl;
        CloseHandle(moduleSnap);
        return 1;
    }

    do
    {
        std::wcout << L"Module Name: " << me.szModule
                   << L", Executable Path: " << me.szExePath
                   << L", Base Address: " << me.modBaseAddr
                   << L", Module Size: " << me.modBaseSize
                   << L", Process ID: " << me.th32ProcessID
                   << L", Reference Count: " << me.GlblcntUsage
                   << L", Process Reference Count: " << me.ProccntUsage
                   << std::endl;
    } while (Module32Next(moduleSnap, &me));
}