#pragma once

#include "NotePage.g.h"

using namespace winrt::Windows::Storage;
using namespace winrt::Windows::Foundation;
using namespace winrt::Microsoft::UI::Xaml;
using namespace winrt::param;

namespace winrt::WinUIHello2::implementation
{
    struct NotePage : NotePageT<NotePage>
    {
        NotePage()
        {
            // Xaml objects should not call InitializeComponent during construction.
            // See https://github.com/microsoft/cppwinrt/tree/master/nuget#initializecomponent

            m_storageFolder = ApplicationData::Current().LocalFolder();

            Loaded({ this, &NotePage::NotePage_Loaded })
        }

        int32_t MyProperty();
        void MyProperty(int32_t value);

        void NotePage_Loaded(const IInspectable& sender, const RoutedEventArgs& e);

    private:
        StorageFolder m_storageFolder{ nullptr };
        StorageFile m_file{ nullptr };
        hstring m_fileName;
    };
}

namespace winrt::WinUIHello2::factory_implementation
{
    struct NotePage : NotePageT<NotePage, implementation::NotePage>
    {
    };
}
 