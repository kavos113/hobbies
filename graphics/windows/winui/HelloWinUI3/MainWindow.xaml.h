#pragma once

#include "MainWindow.g.h"

namespace winrt::HelloWinUI3::implementation
{
    struct MainWindow : MainWindowT<MainWindow>
    {
        MainWindow() : m_images(winrt::single_threaded_observable_vector<Windows::Foundation::IInspectable>())
        {
            // Xaml objects should not call InitializeComponent during construction.
            // See https://github.com/microsoft/cppwinrt/tree/master/nuget#initializecomponent

            InitializeComponent();
            GetItemsAsync();
        }

        Windows::Foundation::Collections::IVector<Windows::Foundation::IInspectable> Images() const 
        {
            return m_images;
        }

    private:
        Windows::Foundation::Collections::IVector<Windows::Foundation::IInspectable> m_images{ nullptr };

        Windows::Foundation::IAsyncAction GetItemsAsync();
        Windows::Foundation::IAsyncOperation<ImageFileInfo> LoadImageInfoAsync(Windows::Storage::StorageFile const& file);
    };
}

namespace winrt::HelloWinUI3::factory_implementation
{
    struct MainWindow : MainWindowT<MainWindow, implementation::MainWindow>
    {
    };
}
