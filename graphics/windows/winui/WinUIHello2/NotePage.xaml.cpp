#include "pch.h"
#include "NotePage.xaml.h"
#if __has_include("NotePage.g.cpp")
#include "NotePage.g.cpp"
#endif

using namespace winrt;
using namespace Microsoft::UI::Xaml;

// To learn more about WinUI, the WinUI project structure,
// and more about our project templates, see: http://aka.ms/winui-project-info.

namespace winrt::WinUIHello2::implementation
{
    int32_t NotePage::MyProperty()
    {
        throw hresult_not_implemented();
    }

    void NotePage::MyProperty(int32_t /* value */)
    {
        throw hresult_not_implemented();
    }
}
