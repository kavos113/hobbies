#pragma once

#include "NotePage.g.h"

namespace winrt::WinUIHello2::implementation
{
    struct NotePage : NotePageT<NotePage>
    {
        NotePage()
        {
            // Xaml objects should not call InitializeComponent during construction.
            // See https://github.com/microsoft/cppwinrt/tree/master/nuget#initializecomponent
        }

        int32_t MyProperty();
        void MyProperty(int32_t value);
    };
}

namespace winrt::WinUIHello2::factory_implementation
{
    struct NotePage : NotePageT<NotePage, implementation::NotePage>
    {
    };
}
