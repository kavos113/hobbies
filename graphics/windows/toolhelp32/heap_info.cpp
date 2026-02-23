#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <tlhelp32.h>
#include <iostream>

int main()
{
    HANDLE heapSnap = CreateToolhelp32Snapshot(TH32CS_SNAPHEAPLIST, 0);
    if (heapSnap == INVALID_HANDLE_VALUE)
    {
        std::cerr << "Failed to create heap snapshot: " << GetLastError() << std::endl;
        return 1;
    }

    HEAPLIST32 hl;
    hl.dwSize = sizeof(HEAPLIST32);

    if (!Heap32ListFirst(heapSnap, &hl))
    {
        std::cerr << "Failed to get first heap: " << GetLastError() << std::endl;
        CloseHandle(heapSnap);
        return 1;
    }

    do
    {
        std::wcout << L"Process ID: " << hl.th32ProcessID
                   << L", Heap ID: " << hl.th32HeapID
                   << L", Flags: " << hl.dwFlags
                   << std::endl;
    } while (Heap32ListNext(heapSnap, &hl));
}