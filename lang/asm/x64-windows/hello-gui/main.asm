default rel

extern DefWindowProcW
extern RegisterClassExW
extern CreateWindowExW
extern GetModuleHandleW
extern LoadIconW
extern LoadCursorW
extern ShowWindow
extern UpdateWindow
extern GetMessageW
extern TranslateMessage
extern DispatchMessageW
extern PostQuitMessage
extern ExitProcess

; WNDCLASSEXW structure
; typedef struct tagWNDCLASSEXW {  off size
;   UINT      cbSize;              0   4
;   UINT      style;               4   4
;   WNDPROC   lpfnWndProc;         8   8
;   int       cbClsExtra;          16  4
;   int       cbWndExtra;          20  4
;   HINSTANCE hInstance;           24  8
;   HICON     hIcon;               32  8
;   HCURSOR   hCursor;             40  8
;   HBRUSH    hbrBackground;       48  8
;   LPCWSTR   lpszMenuName;        56  8
;   LPCWSTR   lpszClassName;       64  8
;   HICON     hIconSm;             72  8
; } WNDCLASSEXW, *PWNDCLASSEXW;

%macro WSTR 2
    %1 dw __utf16__(%2), 0
%endmacro

section .data
    WSTR class_name, "HelloWindowClass"
    WSTR window_title, "Hello, World!"

; VARIABLES
; name type        offset size
; wc   WNDCLASSEXW 0      80
; msg  MSG         80     48

section .text
    global main

; WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lparam)
wnd_proc:
    cmp edx, 0x0012 ; WM_QUIT
    je .quit
    cmp edx, 0x0002 ; WM_DESTROY
    je .destroy
    jmp DefWindowProcW

.destroy:
    sub rsp, 32

    ; PostQuitMessage(0)
    xor rcx, rcx
    call PostQuitMessage

    add rsp, 32
    xor rax, rax
    ret

.quit:
    xor rax, rax
    ret

main:
    sub rsp, 136 ; 8 bytes: return address + 128 bytes for local variables and alignment

    ; Initialize WNDCLASSEXW structure
    lea rdi, [rsp + 32]
    mov dword [rdi], 80 ; cbSize
    mov dword [rdi + 4], 3 ; style = CS_HREDRAW | CS_VREDRAW
    lea rax, [wnd_proc]
    mov [rdi + 8], rax ; lpfnWndProc
    xor rax, rax
    mov [rdi + 16], rax ; cbClsExtra = 0
    mov [rdi + 20], rax ; cbWndExtra = 0

    ; hInstance = GetModuleHandleW(NULL)
    xor rcx, rcx
    call GetModuleHandleW
    mov [rdi + 24], rax ; hInstance

    ; hIcon = LoadIconW(NULL, IDI_APPLICATION)
    xor rcx, rcx
    mov rdx, 32512 ; IDI_APPLICATION
    call LoadIconW
    mov [rdi + 32], rax ; hIcon

    ; hCursor = LoadCursorW(NULL, IDC_ARROW)
    xor rcx, rcx
    mov rdx, 32512 ; IDC_ARROW
    call LoadCursorW
    mov [rdi + 40], rax ; hCursor

    mov [rdi + 48], 6 ; hbrBackgroud = (COLOR_WINDOW + 1)
    
    xor rax, rax
    mov [rdi + 56], rax ; lpszMenuName = NULL
    mov [rdi + 72], rax ; hIconSm = NULL
    
    ; lpszClassName = class_name
    lea rax, [class_name]
    mov [rdi + 64], rax

    mov rcx, rdi
    call RegisterClassExW

    ; CreateWindowExW
    ; ARGUMENTS
    ; name            type      location size
    ; dwExStyle       DWORD     rcx      8
    ; lpClassName     LPCWSTR   rdx      8
    ; lpWindowName    LPCWSTR   r8       8
    ; dwStyle         DWORD     r9       8
    ; x               int       [rsp+32] 4   // 8-byte alignment
    ; y               int       [rsp+40] 4
    ; nWidth          int       [rsp+48] 4
    ; nHeight         int       [rsp+56] 4
    ; hWndParent      HWND      [rsp+64] 8
    ; hMenu           HMENU     [rsp+72] 8
    ; hInstance       HINSTANCE [rsp+80] 8
    ; lpParam         LPVOID    [rsp+88] 8
    xor rcx, rcx ; dwExStyle = 0
    lea rdx, [class_name] ; lpClassName
    lea r8, [window_title] ; lpWindowName
    mov r9, 0x00CF0000 ; dwStyle = WS_OVERLAPPEDWINDOW (0x00CF0000)
    mov dword [rsp + 32], 100 ; x
    mov dword [rsp + 40], 100 ; y
    mov dword [rsp + 48], 800 ; nWidth
    mov dword [rsp + 56], 600 ; nHeight
    xor rax, rax
    mov [rsp + 64], rax ; hWndParent = NULL
    mov [rsp + 72], rax ; hMwnu = NULL

    xor rcx, rcx
    call GetModuleHandleW
    mov [rsp + 80], rax ; hInstance = GetModuleHandle(NULL)

    xor rax, rax
    mov [rsp + 88], rax ; lpParam = NULL
    call CreateWindowExW

    mov rsi, rax ; rsi = hwnd

    ; ShowWindow(hwnd, SW_SHOW)
    mov rcx, rsi
    mov rdx, 5 ; SW_SHOW
    call ShowWindow

    ; UpdateWindow(hwnd, SW_SHOW)
    mov rcx, rsi
    call UpdateWindow

    ; msg = 0
    xor rax, rax
    mov [rsp + 64], rax
    lea rdi, [rsp + 64]
.msg_loop:
    ; GetMessageW(&msg, NULL, 0, 0)
    mov rcx, rdi
    xor rdx, rdx
    xor r8, r8
    xor r9, r9
    call GetMessageW

    test rax, rax
    jz .end_loop

    ; TranslateMessage(&msg)
    mov rcx, rdi
    call TranslateMessage

    ; DispatchMessageW(&msg)
    mov rcx, rdi
    call DispatchMessageW

    jmp .msg_loop

.end_loop:
    xor rcx, rcx
    call ExitProcess

    add rsp, 136
    ret