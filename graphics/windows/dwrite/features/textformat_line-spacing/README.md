# IDWriteTextFormat::SetLineSpacing

## MSDN
- [IDWriteTextFormat::SetLineSpacing method](https://docs.microsoft.com/en-us/windows/win32/api/dwrite/nf-dwrite-idwritetextformat-setlinespacing)
- [DWRITE_LINE_SPACING enumeration](https://docs.microsoft.com/en-us/windows/win32/api/dwrite/ne-dwrite-dwrite_line_spacing)

## Description
```cpp
HRESULT SetLineSpacing(DWRITE_LINE_SPACING_METHOD lineSpacingMethod, FLOAT lineSpacing, FLOAT baseline);

typedef enum DWRITE_LINE_SPACING_METHOD {
  DWRITE_LINE_SPACING_METHOD_DEFAULT,
  DWRITE_LINE_SPACING_METHOD_UNIFORM,
  DWRITE_LINE_SPACING_METHOD_PROPORTIONAL
} ;
```

## Result

note: font size = 32.0f

UNIFORM, lineSpacing = 15.0f, baseline = 0.0f
![](images/capture0.png)

UNIFORM, lineSpacing = 15.0f, baseline = 15.0f
![](images/capture1.png)

UNIFORM, lineSpacing = 30.0f, baseline = 0.0f
![](images/capture2.png)

UNIFORM, lineSpacing = 30.0f, baseline = 30.0f
![](images/capture3.png)

UNIFORM, lineSpacing = 30.0f, baseline = 60.0f
![](images/capture4.png)

DEFAULT, lineSpacing = 1.0f, baseline = 0.0f
![](images/capture5.png)

DEFAULT, lineSpacing = 2.0f, baseline = 2.0f
![](images/capture6.png)

PROPORTIONAL, lineSpacing = 1.0f, baseline = 0.0f
![](images/capture7.png)

PROPORTIONAL, lineSpacing = 2.0f, baseline = 2.0f
![](images/capture8.png)

PROPORTIONAL, lineSpacing = 2.0f, baseline = 4.0f
![](images/capture9.png)