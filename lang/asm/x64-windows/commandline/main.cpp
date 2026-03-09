#include <Windows.h>

int main()
{
    const wchar_t *cap = L"Command Line Arguments";
    const wchar_t *fmt = L"Argument %d: %s\n";

    wchar_t buf[256];
    int argc;

    LPWSTR cmdLine = GetCommandLineW();
    LPWSTR *argv = CommandLineToArgvW(cmdLine, &argc);

    for (int i = 0; i < argc; i++)
    {
        wsprintfW(buf, fmt, i, argv[i]);
        MessageBoxW(NULL, buf, cap, MB_OK);
    }
}