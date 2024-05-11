        .section        __TEXT,__text
        .globl          _putc

_putc:
    leaq    (%rsp),      %rsi
    movq    $0x2000004,   %rax
    movq    $1,           %rdi
    movq    %rdi,         %rdx
    syscall
    jmpq *%r12

        .section        __DATA,__data
        .globl          _stack
_stack_end:   .space    8*4096
_stack:       .quad     . - _stack_end
