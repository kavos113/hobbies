default rel

extern MessageBoxA

section .data
    caption db 'Hello, WinAPI!', 0
    message db 'Hello, World!', 0

section .text
    global main

main:
    sub rsp, 40

    ; Call MessageBoxA(NULL, message, caption, MB_OK)
    xor rcx, rcx        ; hWnd = NULL
    lea rdx, [message]  ; lpText = message
    lea r8,  [caption]  ; lpCaption = caption
    xor r9,  r9         ; uType = MB_OK(0)
    call MessageBoxA

    add rsp, 40
    ret