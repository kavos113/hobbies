#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <winhttp.h>
#include <iostream>
#include <vector>
#include <string>

int main()
{
    HINTERNET internet = WinHttpOpen(
        L"UserAgent/1.0",
        WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY,
        WINHTTP_NO_PROXY_NAME,
        WINHTTP_NO_PROXY_BYPASS,
        0
    );
    if (internet == nullptr)
    {
        std::cerr << "failed to open winhttp: " << GetLastError() << std::endl;
        return 1;
    }

    HINTERNET session = WinHttpConnect(internet, L"www.google.com", INTERNET_DEFAULT_HTTPS_PORT, 0);
    if (session == nullptr)
    {
        std::cerr << "failed to connect to google: " << GetLastError() << std::endl;
        WinHttpCloseHandle(internet);
        return 1;
    }

    HINTERNET request = WinHttpOpenRequest(
        session,
        L"GET",
        L"/index.html",
        nullptr, // HTTP 1.1
        WINHTTP_NO_REFERER,
        WINHTTP_DEFAULT_ACCEPT_TYPES,
        WINHTTP_FLAG_SECURE
    );
    if (request == nullptr)
    {
        std::cerr << "failed to open request: " << GetLastError() << std::endl;
        WinHttpCloseHandle(session);
        WinHttpCloseHandle(internet);
        return 1;
    }

    BOOL result = WinHttpSendRequest(
        request,
        WINHTTP_NO_ADDITIONAL_HEADERS, 0,
        WINHTTP_NO_REQUEST_DATA, 0,
        0, 0
    );
    if (!result)
    {
        std::cerr << "failed to send request: " << GetLastError() << std::endl;
        WinHttpCloseHandle(request);
        WinHttpCloseHandle(session);
        WinHttpCloseHandle(internet);
        return 1;
    }

    result = WinHttpReceiveResponse(request, 0);
    if (!result)
    {
        std::cerr << "failed to recieve response: " << GetLastError() << std::endl;
        WinHttpCloseHandle(request);
        WinHttpCloseHandle(session);
        WinHttpCloseHandle(internet);
        return 1;
    }

    DWORD size = 0;
    DWORD actualSize = 0;
    std::string response;
    do
    {
        size = 0;
        result = WinHttpQueryDataAvailable(request, &size);
        if (!result)
        {
            std::cerr << "failed to query data: " << GetLastError() << std::endl;
            break;
        }

        if (size == 0)
        {
            break;
        }

        std::vector<char> buf(size + 1);
        result = WinHttpReadData(request, buf.data(), size, &actualSize);
        if (!result)
        {
            std::cerr << "failed to read data: " << GetLastError() << std::endl;
            break;
        }

        response.append(buf.data(), actualSize);
    } while (size > 0);

    std::cout << "response: " << response << std::endl;

    WinHttpCloseHandle(request);
    WinHttpCloseHandle(session);
    WinHttpCloseHandle(internet);
    return 0;
}