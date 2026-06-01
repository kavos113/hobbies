#ifndef WIN32_02_SPLIT_CLASS_APPLICATION_H
#define WIN32_02_SPLIT_CLASS_APPLICATION_H

#ifndef UNICODE
#define UNICODE
#endif

#include <windows.h>
#include <d2d1_1.h>
#include <wrl/client.h>

class Application
{
public:
    Application(int width = 800, int height = 600);
    ~Application();

    void initD2D();

    void run(const wchar_t* outputFilename = L"output.png");

private:
    void onPaint();
    void saveImage(const wchar_t* filename);

    void createSurfaceBitmap();
    void createResources();

    RECT m_windowRect;

    Microsoft::WRL::ComPtr<ID2D1Factory1> m_d2dFactory;
    Microsoft::WRL::ComPtr<ID2D1Device> m_d2dDevice;
    Microsoft::WRL::ComPtr<ID2D1DeviceContext> m_d2dContext;
    Microsoft::WRL::ComPtr<ID2D1Bitmap1> m_bitmap;
    Microsoft::WRL::ComPtr<ID2D1SolidColorBrush> m_brush;
    Microsoft::WRL::ComPtr<ID2D1SolidColorBrush> m_greenBrush;
    Microsoft::WRL::ComPtr<ID2D1StrokeStyle> m_strokeStyle;
};



#endif //WIN32_02_SPLIT_CLASS_APPLICATION_H
