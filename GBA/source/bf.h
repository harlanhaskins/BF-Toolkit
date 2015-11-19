#ifndef __bf_h_
#define __bf_h_

#define STACK_SIZE 512

typedef struct Instruction {
    char operator;
    int operand;
} Instruction;

int compile_bf(char *input, int size, Instruction **out);
void run_bf(char *memory_space, int memory_size, Instruction *program, int prog_size, void (*out)(char c), char (*in)());

#endif // __bf_h_
