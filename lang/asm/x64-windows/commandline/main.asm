default rel

extern GetCommandLineW
extern CommandLineToArgvW
extern LocalFree
extern MessageBoxW
extern wsprintfW
extern ExitProcess

section .data
    cap dw __utf16__("Command Line Arguments"), 0
    fmt dw __utf16__("Argument %d: %s"), 0

section .bss
    buf resb 256
    argc resd 1

section .text
    global main

main:
    sub rsp, 40 ; Shadow space for Windows x64 calling convention

    ; LPWSTR lpCmdLine = GetCommandLineW();
    call GetCommandLineW
    mov rcx, rax ; rcx = lpCmdLine

    ; int argc
    ; LPWSTR *argv = CommandLineToArgvW(lpCmdLine, &argc);
    lea rdx, [argc] 
    call CommandLineToArgvW
    mov rbx, rax ; rbx = argv
    mov rdi, [argc] ; rdi = argc

    ; Display each argument in a message box
    xor rsi, rsi ; loop counter
.arg_loop:
    cmp rsi, rdi    
    jge .end_loop

    ; Prepare the message for MessageBoxW
    lea rcx, [buf]
    lea rdx, [fmt]
    mov r8, rsi
    mov r9, [rbx + rsi * 8] ; r9 = argv[i]
    call wsprintfW

    ; Call MessageBoxW(NULL, buf, cap, MB_OK)
    xor rcx, rcx
    lea rdx, [buf]
    lea r8,  [cap]
    xor r9,  r9
    call MessageBoxW

    inc rsi
    jmp .arg_loop

.end_loop:
    mov rcx, rbx
    call LocalFree

    xor rcx, rcx
    call ExitProcess

    add rsp, 40 ; Restore stack pointer
    ret