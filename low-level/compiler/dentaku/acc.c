#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    if (argc != 2) 
    {
        fprintf(stderr, "arg: number");
        return 1;
    }

    char *str = argv[1];

    printf(".intel_syntax noprefix\n");
    printf(".global main\n");
    printf("main:\n");
    printf("  mov rax, %ld\n", strtol(str, &str, 10));

    while (*str) 
    {
        if (*str == '+')
        {
            str++;
            printf("  add rax, %ld\n", strtol(str, &str, 10));
            continue;
        }

        if (*str == '-')
        {
            str++;
            printf("  sub rax, %ld\n", strtol(str, &str, 10));
            continue;
        }

        fprintf(stderr, "unknown char: %c\n", *str);
        return 1;
    }

    printf("  ret\n");
    return 0;
}