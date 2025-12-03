# IDWriteTextFormat::SetTextAlignment

## MSDN
- [IDWriteTextFormat::SetTextAlignment method](https://docs.microsoft.com/en-us/windows/win32/api/dwrite/nf-dwrite-idwritetextformat-settextalignment)
- [DWRITE_TEXT_ALIGNMENT enumeration](https://docs.microsoft.com/en-us/windows/win32/api/dwrite/ne-dwrite-dwrite_text_alignment)

## Description
```cpp
HRESULT SetTextAlignment(DWRITE_TEXT_ALIGNMENT textAlignment);

typedef enum DWRITE_TEXT_ALIGNMENT {
  DWRITE_TEXT_ALIGNMENT_LEADING,
  DWRITE_TEXT_ALIGNMENT_TRAILING,
  DWRITE_TEXT_ALIGNMENT_CENTER,
  DWRITE_TEXT_ALIGNMENT_JUSTIFIED
} ;
```

## Result
LEADING
![](images/capture0.png)

TRAILING
![](images/capture1.png)

CENTER
![](images/capture2.png)

JUSTIFIED
![](images/capture3.png)

ReadingDirection: RightToLeft, TextAlignment: LEADING
![](images/capture4.png)

ReadingDirection: RightToLeft, TextAlignment: TRAILING
![](images/capture5.png)