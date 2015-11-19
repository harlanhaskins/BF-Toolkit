#include <string.h>
#include "bf.h"

int compile_bf(char *input, int size, Instruction **out) {
    Instruction *program = *out;
    char *c = input;
    int pc = 0, jmp_pc = 0;
    int stack[STACK_SIZE];
    int sp = 0;
    while (*c != '\0' && pc < size) {
        switch (*c) {
            case '+':
            case '-':
            case '<':
            case '>':
            case '.':
            case ',':
                program[pc].operator = *c;
                break;
            case '[':
                program[pc].operator = *c;
                if (sp >= STACK_SIZE - 1) {
                    return 1;
                }
                stack[sp++] = pc;
                break;
            case ']':
                if (sp <= 0) {
                    return 1;
                }
                jmp_pc = stack[--sp];
                program[pc].operator = *c;
                program[pc].operand = jmp_pc;
                program[jmp_pc].operand = pc;
                break;
            default: pc--; break;
        }
        pc++;
        c++;
    }
    if (sp != 0) {
        return 1;
    }
    program[pc].operator = -1;
    *out = program;
    return 0;
}

void run_bf(char *memory_space, int memory_size, Instruction *program, int prog_size, void (*out)(char c), char (*in)()) {
    char *p = memory_space;
    int pc = 0;
    while (pc < prog_size && p < (memory_space + memory_size)) {
        switch (program[pc].operator) {
            case '+': ++*p; break;
            case '-': --*p; break;
            case '>': ++p; break;
            case '<': --p; break;
            case '.': out(*p); break;
            case ',': *p = in(); break;
            case '[': if (!*p) { pc = program[pc].operand; } break;
            case ']': if (*p) { pc = program[pc].operand; } break;
            default: break;
        }
        pc++;
    }
}
