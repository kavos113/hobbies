#ifndef WIN32_02_SPLIT_CLASS_APPLICATION_H
#define WIN32_02_SPLIT_CLASS_APPLICATION_H

#include <memory>

#ifndef UNICODE
#define UNICODE
#endif
#include <windows.h>

#include "D3DEngine.h"
#include "D3DContext.h"

class Application
{
public:
    Application();
    ~Application();

    int createWindow(int x = CW_USEDEFAULT, int y = CW_USEDEFAULT, int width = 800, int height = 600);

    void run();

private:
    static LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
    LRESULT handleMessage(UINT uMsg, WPARAM wParam, LPARAM lParam);

    std::unique_ptr<D3DEngine> m_engine;
    std::unique_ptr<D3DContext> m_context;
    HWND m_hwnd;

    const wchar_t* className = L"ApplicationWindowClass";
};



#endif //WIN32_02_SPLIT_CLASS_APPLICATION_H
