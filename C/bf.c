#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define STACK_SIZE 512

typedef struct Instruction {
    char operator;
    int operand;
} Instruction;

int compile_bf(char *input, Instruction **out) {
    size_t program_size = strlen(input);
    Instruction *program = calloc(program_size, sizeof(Instruction));
    char *c = input;
    int pc = 0, jmp_pc = 0;
    int stack[STACK_SIZE];
    int sp = 0;
    int retval = 0;
    while (*c != '\0' && pc < program_size) {
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
                    puts("Max loop depth exceeded.");
                    return 1;
                }
                stack[sp++] = pc;
                break;
            case ']':
                if (sp <= 0) {
                    puts("Imbalanced loops.");
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
        puts("Finished processing with a non-empty stack.");
        return 1;
    }
    program[pc].operator = EOF;
    *out = program;
    return 0;
}

void run_bf(char *memory_space, size_t memory_size, Instruction *program) {
    char *p = memory_space;
    size_t pc = 0;
    while (program[pc].operator != EOF && p < (memory_space + memory_size)) {
        switch (program[pc].operator) {
            case '+': ++*p; break;
            case '-': --*p; break;
            case '>': ++p; break;
            case '<': --p; break;
            case '.': putchar(*p); break;
            case ',': *p = getchar(); break;
            case '[': if (!*p) { pc = program[pc].operand; } break;
            case ']': if (*p) { pc = program[pc].operand; } break;
            default: break;
        }
        pc++;
    }
}

char *read_string(FILE *fp) {
    size_t max_size = 256 * sizeof(char);
    char *program = malloc(max_size);
    char c;
    size_t counter = 0;
    while ((c = getc(fp)) != EOF) {
        if (counter >= max_size) {
            max_size *= 2;
            program = realloc(program, max_size);
        }
        program[counter] = c;
        counter++;
    }
    return program;
}

int main(int argc, char *argv[]) {
    size_t memory_size = 30000;
    char *memory_space = calloc(memory_size, sizeof(char));
    Instruction *list;
    FILE *fp = fopen(argv[1], "r");
    char *program = read_string(fp);
    fclose(fp);
    if (compile_bf(program, &list)) {
        puts("FAIL");
        return 1;
    }
    run_bf(memory_space, memory_size, list);
    free(list);
    free(memory_space);
    return 0;
}
