#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <lmcons.h>

#include <iostream>

int main()
{
    wchar_t user_name[UNLEN + 1];
    DWORD size = UNLEN + 1;

    BOOL result = GetUserName(user_name, &size);
    if (!result)
    {
        if (GetLastError() == ERROR_INSUFFICIENT_BUFFER)
        {
            std::cerr << "failed to get user name due to insufficient buffer" << std::endl;
        }
        else
        {
            std::cerr << "failed to get user name: " << GetLastError() << std::endl;
        }
        return 1;
    }

    std::wcout << L"User Name: " << user_name << L"\nmax length: " << UNLEN + 1 << std::endl;
    return 0;
}