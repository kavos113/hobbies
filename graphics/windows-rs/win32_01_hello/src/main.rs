use windows::core::w;
use windows::Win32::Foundation::{HWND, LPARAM, LRESULT, WPARAM};
use windows::Win32::Graphics::Gdi::{
    GetSysColorBrush, UpdateWindow, COLOR_WINDOW, SYS_COLOR_INDEX,
};
use windows::Win32::System::LibraryLoader::GetModuleHandleW;
use windows::Win32::UI::WindowsAndMessaging::{
    CreateWindowExW, DefWindowProcW, DispatchMessageW, GetMessageW, LoadCursorW, LoadIconW,
    PostQuitMessage, RegisterClassExW, ShowWindow, TranslateMessage, CS_HREDRAW, CS_VREDRAW,
    IDC_ARROW, IDI_APPLICATION, MSG, SW_SHOW, WINDOW_EX_STYLE, WM_DESTROY, WM_PAINT, WNDCLASSEXW,
    WS_OVERLAPPEDWINDOW,
};

fn main() {
    unsafe {
        let instance = GetModuleHandleW(None).unwrap();
        let window_class = w!("my_window_class");

        let wc = WNDCLASSEXW {
            cbSize: size_of::<WNDCLASSEXW>() as u32,
            style: CS_HREDRAW | CS_VREDRAW,
            lpfnWndProc: Some(wndproc),
            hInstance: instance.into(),
            hIcon: LoadIconW(None, IDI_APPLICATION).unwrap(),
            hCursor: LoadCursorW(None, IDC_ARROW).unwrap(),
            hbrBackground: GetSysColorBrush(SYS_COLOR_INDEX(COLOR_WINDOW.0 + 1)),
            lpszClassName: window_class,
            ..Default::default()
        };

        let atom = RegisterClassExW(&wc);
        if atom == 0 {
            panic!("Failed to register window class");
        }

        let hwnd = match CreateWindowExW(
            WINDOW_EX_STYLE::default(),
            window_class,
            w!("Hello, Windows!"),
            WS_OVERLAPPEDWINDOW,
            100,
            100,
            800,
            600,
            None,
            None,
            Some(instance.into()),
            None,
        ) {
            Ok(hwnd) => hwnd,
            Err(e) => panic!("Failed to create window: {:?}", e),
        };

        ShowWindow(hwnd, SW_SHOW);
        UpdateWindow(hwnd);

        let mut msg = MSG::default();
        while GetMessageW(&mut msg, None, 0, 0).into() {
            let _ = TranslateMessage(&msg);
            DispatchMessageW(&msg);
        }
    }
}

extern "system" fn wndproc(hwnd: HWND, msg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
    unsafe {
        match msg {
            WM_PAINT => {
                println!("WM_PAINT received");
                LRESULT(0)
            }

            WM_DESTROY => {
                println!("WM_DESTROY received");
                PostQuitMessage(0);
                LRESULT(0)
            }

            _ => DefWindowProcW(hwnd, msg, wparam, lparam),
        }
    }
}
