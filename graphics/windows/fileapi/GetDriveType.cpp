#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <iostream>

void format_drive_type(UINT type)
{
    switch (type)
    {
    case DRIVE_UNKNOWN:
        std::cout << "unknown drive" << std::endl;
        return;

    case DRIVE_NO_ROOT_DIR:
        std::cout << "invalid root path" << std::endl;
        return;

    case DRIVE_REMOVABLE:
        std::cout << "removable drive" << std::endl;
        return;

    case DRIVE_FIXED:
        std::cout << "fixed drive" << std::endl;
        return;

    case DRIVE_REMOTE:
        std::cout << "network drive" << std::endl;
        return;

    case DRIVE_CDROM:
        std::cout << "CD-ROM drive" << std::endl;
        return;

    case DRIVE_RAMDISK:
        std::cout << "RAM Disk drive" << std::endl;
        return;

    default:
        std::cout << "unknown drive" << std::endl;
        return;
    }
}

int main()
{
    wchar_t rootPath[4] = {};
    rootPath[1] = L':';
    rootPath[2] = L'\\';

    for (wchar_t code = L'A'; code <= 'Z'; code++)
    {
        rootPath[0] = code;

        UINT type = GetDriveType(rootPath);
        std::wcout << rootPath << "  ";
        format_drive_type(type);
    }
}