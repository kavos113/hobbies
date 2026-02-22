#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <iostream>

int main()
{
    wchar_t computer_name[MAX_COMPUTERNAME_LENGTH + 1];
    DWORD size = MAX_COMPUTERNAME_LENGTH + 1;

    BOOL result = GetComputerName(computer_name, &size);
    if (!result)
    {
        if (GetLastError() == ERROR_BUFFER_OVERFLOW)
        {
            std::cerr << "failed to get computer name because buffer too small" << std::endl;
        }
        else
        {
            std::cerr << "failed to get computer name: " << GetLastError() << std::endl;
        }
        return 1;
    }

    std::wcout << L"Computer Name: " << computer_name << std::endl;
    return 0;
}