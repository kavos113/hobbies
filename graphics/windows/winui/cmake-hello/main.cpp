#ifndef UNICODE
#define UNICODE
#endif
#include <windows.h>

#include <winrt/windows.foundation.collections.h>
#include <winrt/windows.ui.xaml.interop.h>
#include <winrt/Microsoft.UI.Xaml.Controls.h>
#include <winrt/Microsoft.UI.Xaml.Controls.Primitives.h>
#include <winrt/Microsoft.UI.Xaml.XamlTypeInfo.h>
#include <winrt/Microsoft.UI.Xaml.Markup.h>

using namespace winrt;
using namespace Microsoft::UI::Xaml;
using namespace Microsoft::UI::Xaml::Controls;
using namespace Microsoft::UI::Xaml::XamlTypeInfo;
using namespace Microsoft::UI::Xaml::Markup;
using namespace Windows::UI::Xaml::Interop;
using namespace Windows::Foundation;

class MainWindow : public WindowT<MainWindow>
{
public:
    MainWindow()
    {
        StackPanel stackPanel;
        stackPanel.HorizontalAlignment(HorizontalAlignment::Center);
        stackPanel.VerticalAlignment(VerticalAlignment::Center);

        TextBlock title;
        title.Style(Application::Current().Resources().Lookup(box_value(L"TitleTextBlockStyle")).as<Style>());
        title.Text(L"WinUI 3 in C++ Without XAML!");
        title.HorizontalAlignment(HorizontalAlignment::Center);

        HyperlinkButton project;
        project.Content(box_value(L"Github Project Repository"));
        project.NavigateUri(Uri(L"https://github.com/sotanakamura/winui3-without-xaml"));
        project.HorizontalAlignment(HorizontalAlignment::Center);

        Button button;
        button.Content(box_value(L"Click"));
        button.Click([&](IInspectable const& sender, RoutedEventArgs) { sender.as<Button>().Content(box_value(L"Thank You!")); });
        button.HorizontalAlignment(HorizontalAlignment::Center);
        button.Margin(ThicknessHelper::FromUniformLength(20));

        Content(stackPanel);
        stackPanel.Children().Append(title);
        stackPanel.Children().Append(project);
        stackPanel.Children().Append(button);
    }
};

class App : public ApplicationT<App, IXamlMetadataProvider>
{
public:
    void OnLaunched(const LaunchActivatedEventArgs &args)
    {
        Resources().MergedDictionaries().Append(XamlControlsResources());

        m_window = make<MainWindow>();
        m_window.Activate();
    }

    IXamlType GetXamlType(const TypeName& type)
    {
        return m_provider.GetXamlType(type);
    }

    IXamlType GetXamlType(const hstring& fullname)
    {
        return m_provider.GetXamlType(fullname);
    }

    com_array<XmlnsDefinition> GetXmlnsDefinitions()
    {
        return m_provider.GetXmlnsDefinitions();
    }

private:
    Window m_window{nullptr};
    XamlControlsXamlMetaDataProvider m_provider;
};

int WINAPI wWinMain(HINSTANCE hinstance, HINSTANCE hPrevInstance, LPWSTR pCmdLine, int nCmdShow)
{
    init_apartment();
    Application::Start([](auto&&)
    {
       make<App>();
    });
    return 0;
}