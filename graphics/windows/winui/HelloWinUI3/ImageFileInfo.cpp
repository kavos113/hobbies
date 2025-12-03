#include "pch.h"
#include "ImageFileInfo.h"

#include <random>

#include "ImageFileInfo.g.cpp"

namespace winrt::HelloWinUI3::implementation
{
    ImageFileInfo::ImageFileInfo(
        Windows::Storage::FileProperties::ImageProperties const& properties,
        Windows::Storage::StorageFile const& imageFile,
        hstring const& name,
        hstring const& type
    )
    : m_imageProperties(properties)
    , m_imageFile(imageFile)
    , m_imageName(name)
    , m_imageFileType(type)
    {
        auto rating = properties.Rating();
        auto random = std::random_device();
        std::uniform_int_distribution<uint32_t> distribution(1, 5);
        ImageRating(rating == 0 ? distribution(random) : rating);
    }

    void ImageFileInfo::ImageTitle(hstring const& value)
    {
        if (ImageProperties().Title() != value)
        {
            ImageProperties().Title(value);
            ImageProperties().SavePropertiesAsync();
            OnPropertyChanged(L"ImageTitle");
        }
    }

    void ImageFileInfo::ImageRating(uint32_t value)
    {
        if (ImageProperties().Rating() != value)
        {
            ImageProperties().Rating(value);
            ImageProperties().SavePropertiesAsync();
            OnPropertyChanged(L"ImageRating");
        }
    }

    Windows::Foundation::IAsyncOperation<Microsoft::UI::Xaml::Media::Imaging::BitmapImage> ImageFileInfo::
    GetImageStreamAsync()
    {
        Windows::Storage::Streams::IRandomAccessStream stream{ co_await ImageFile().OpenAsync(Windows::Storage::FileAccessMode::Read) };
        Microsoft::UI::Xaml::Media::Imaging::BitmapImage image;
        image.SetSource(stream);
        co_return image;
    }

    Windows::Foundation::IAsyncOperation<Microsoft::UI::Xaml::Media::Imaging::BitmapImage> ImageFileInfo::
    GetImageThumbailAsync()
    {
        auto thumbnail = co_await ImageFile().GetThumbnailAsync(Windows::Storage::FileProperties::ThumbnailMode::PicturesView);
        Microsoft::UI::Xaml::Media::Imaging::BitmapImage image;
        image.SetSource(thumbnail);
        thumbnail.Close();
        co_return image;
    }
}
