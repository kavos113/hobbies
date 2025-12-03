#include "DWrite.h"

#include <iostream>

void DWrite::Change()
{
    // change this function

    m_actions.push_back([this]()
    {
        m_textLayout->SetWordWrapping(DWRITE_WORD_WRAPPING_WRAP);
    });
    m_actions.push_back([this]()
    {
        m_textLayout->SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP);
    });
    m_actions.push_back([this]()
    {
        m_textLayout->SetWordWrapping(DWRITE_WORD_WRAPPING_EMERGENCY_BREAK);
    });
    m_actions.push_back([this]()
    {
        m_textLayout->SetWordWrapping(DWRITE_WORD_WRAPPING_WHOLE_WORD);
    });
    m_actions.push_back([this]()
    {
        m_textLayout->SetWordWrapping(DWRITE_WORD_WRAPPING_CHARACTER);
    });
}

int DWrite::NextAction()
{
    if (m_currentAction >= m_actions.size())
    {
        return -1;
    }

    m_actions[m_currentAction]();
    m_currentAction++;

    return m_currentAction - 1;
}

void DWrite::Init(const D2D1_RECT_F rect, const Microsoft::WRL::ComPtr<ID2D1DeviceContext7> &d2dContext)
{
    m_rect = rect;
    m_d2dContext = d2dContext;

    HRESULT hr = DWriteCreateFactory(
        DWRITE_FACTORY_TYPE_SHARED,
        __uuidof(IDWriteFactory8),
        &m_dwriteFactory
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to create DWrite factory" << std::endl;
        return;
    }

    hr = m_dwriteFactory->CreateTextFormat(
        L"Segoe UI",
        nullptr,
        DWRITE_FONT_WEIGHT_REGULAR,
        DWRITE_FONT_STYLE_NORMAL,
        DWRITE_FONT_STRETCH_NORMAL,
        32.0f,
        L"en-US",
        &m_textFormat
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to create text format" << std::endl;
        return;
    }

    hr = m_dwriteFactory->CreateTextLayout(
        L"This is a sample of text renderedeeeeeeee using DirectWrite.\n\nThis is a new line.",
        wcslen(L"This is a sample of text renderedeeeeeeee using DirectWrite.\n\nThis is a new line."),
        m_textFormat.Get(),
        m_rect.right - m_rect.left,
        m_rect.bottom - m_rect.top,
        &m_textLayout
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to create text layout" << std::endl;
        return;
    }

    hr = m_d2dContext->CreateSolidColorBrush(
        D2D1::ColorF(D2D1::ColorF::Black),
        &m_brush
    );
    if (FAILED(hr))
    {
        std::cerr << "Failed to create solid color brush" << std::endl;
        return;
    }

    Change();
}

void DWrite::Render()
{
    m_d2dContext->DrawTextLayout(
        D2D1::Point2F(m_rect.left, m_rect.top),
        m_textLayout.Get(),
        m_brush.Get()
    );
}
