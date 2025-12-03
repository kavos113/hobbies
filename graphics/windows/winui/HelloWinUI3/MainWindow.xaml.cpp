#include "pch.h"
#include "MainWindow.xaml.h"
#if __has_include("MainWindow.g.cpp")
#include "MainWindow.g.cpp"
#endif

using namespace winrt;
using namespace Microsoft::UI::Xaml;

// To learn more about WinUI, the WinUI project structure,
// and more about our project templates, see: http://aka.ms/winui-project-info.

namespace winrt::HelloWinUI3::implementation
{
    Windows::Foundation::IAsyncAction MainWindow::GetItemsAsync()
    {
        Windows::Storage::StorageFolder appInstalledFolder = Windows::ApplicationModel::Package::Current().InstalledLocation();
        Windows::Storage::StorageFolder picturesFolder = co_await appInstalledFolder.GetFolderAsync(L"Assets\\Samples");

        auto result = picturesFolder.CreateFileQueryWithOptions(Windows::Storage::Search::QueryOptions());

        Windows::Foundation::Collections::IVectorView<Windows::Storage::StorageFile> files = co_await result.GetFilesAsync();
        for (auto const& file : files)
        {
            Images().Append(co_await LoadImageInfoAsync(file));
        }

        ImageGridView().ItemsSource(Images());
    }

    Windows::Foundation::IAsyncOperation<HelloWinUI3::ImageFileInfo> MainWindow::LoadImageInfoAsync(
        Windows::Storage::StorageFile const& file)
    {
        auto properties = co_await file.Properties().GetImagePropertiesAsync();
        ImageFileInfo imageInfo(properties, file, file.DisplayName(), file.DisplayType());

        co_return imageInfo;
    }
}
