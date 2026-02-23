#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <tlhelp32.h>
#include <iostream>

int main()
{
    HANDLE snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if (snapshot == INVALID_HANDLE_VALUE)
    {
        std::cerr << "Failed to create snapshot: " << GetLastError() << std::endl;
        return 1;
    }

    PROCESSENTRY32 pe;
    pe.dwSize = sizeof(PROCESSENTRY32);

    BOOL res = Process32First(snapshot, &pe);
    if (!res)
    {
        std::cerr << "Failed to get first process: " << GetLastError() << std::endl;
        CloseHandle(snapshot);
        return 1;
    }

    do
    {
        std::wcout << L"Process ID: " << pe.th32ProcessID
                   << L", Executable: " << pe.szExeFile
                   << L", Parent Process ID: " << pe.th32ParentProcessID
                   << L", Thread Count: " << pe.cntThreads
                            << std::endl;
    } while (Process32Next(snapshot, &pe));
}