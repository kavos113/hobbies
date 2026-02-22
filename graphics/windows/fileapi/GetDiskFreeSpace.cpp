#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <iostream>
#include <string>
#include <format>

std::string format_memory_size(ULARGE_INTEGER size)
{
    const char* suffixes[] = { "B", "KB", "MB", "GB", "TB", "PB", "EB" };
    int suffix_index = 0;
    double display_size = static_cast<double>(size.QuadPart);

    while (display_size >= 1024 && suffix_index < _countof(suffixes) - 1)
    {
        display_size /= 1024;
        ++suffix_index;
    }

    return std::format("{:.2f} {}", display_size, suffixes[suffix_index]);
}

int main()
{
    ULARGE_INTEGER freeBytes;
    ULARGE_INTEGER totalBytes;
    ULARGE_INTEGER globalFreeBytes;
    BOOL result = GetDiskFreeSpaceEx(L"C:\\Users", &freeBytes, &totalBytes, &globalFreeBytes);
    if (!result)
    {
        std::cerr << "get disk free space failed: " << GetLastError() << std::endl;
        return 1;
    }

    std::cout << "Free Bytes Available to Caller: " << format_memory_size(freeBytes) << std::endl;
    std::cout << "Total Number of Bytes: " << format_memory_size(totalBytes) << std::endl;
    std::cout << "Total Number of Free Bytes: " << format_memory_size(globalFreeBytes) << std::endl;
    return 0;
}