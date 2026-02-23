#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <tlhelp32.h>
#include <iostream>

int main()
{
    HANDLE threadSnap = CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
    if (threadSnap == INVALID_HANDLE_VALUE)
    {
        std::cerr << "Failed to create thread snapshot: " << GetLastError() << std::endl;
        return 1;
    }

    THREADENTRY32 te;
    te.dwSize = sizeof(THREADENTRY32);

    if (!Thread32First(threadSnap, &te))
    {
        std::cerr << "Failed to get first thread: " << GetLastError() << std::endl;
        CloseHandle(threadSnap);
        return 1;
    }

    do
    {
        std::wcout << L"Thread ID: " << te.th32ThreadID
                   << L", Owner Process ID: " << te.th32OwnerProcessID
                   << L", Base Priority: " << te.tpBasePri
                   << std::endl;
    } while (Thread32Next(threadSnap, &te));
}