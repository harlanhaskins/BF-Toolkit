
#include <gba_console.h>
#include <gba_video.h>
#include <gba_interrupt.h>
#include <gba_systemcalls.h>
#include <gba_input.h>
#include <stdio.h>
#include <stdlib.h>
#include "bf.h"

void output_char(char c) {
    iprintf("%c", c);
}

char input_char() {
    return '4';
}

void backspace() {
    iprintf("\x1b[1D \x1b[1D");
}

int main(void) {
    irqInit();
    irqEnable(IRQ_VBLANK);

    consoleDemoInit();

    while (1) {
        iprintf("bf> ");
        char *inst_str = calloc(1000, sizeof(char));
        char *curr = inst_str;

        while (1) {
            scanKeys();
            u16 keys = keysDown();
            if (keys & KEY_START) {
                *curr = '\0';
                iprintf("\n");
                int size = curr - inst_str;
                char *memory_space = calloc(100, sizeof(char));
                Instruction *insts = calloc(size, sizeof(Instruction));
                compile_bf(inst_str, size, &insts);
                run_bf(memory_space, 256, insts, size, output_char, input_char);
                free(insts);
                free(memory_space);
                iprintf("\n");
                break;
            }
            char c = 0;
            if (keys & KEY_UP) {
                c = '+';
            } else if (keys & KEY_DOWN) {
                c = '-';
            } else if (keys & KEY_LEFT) {
                c = '<';
            } else if (keys & KEY_RIGHT) {
                c = '>';
            } else if (keys & KEY_L) {
                c = '[';
            } else if (keys & KEY_R) {
                c = ']';
            } else if (keys & KEY_A) {
                c = '.';
            } else if (keys & KEY_B) {
                c = ',';
            } else if (keys & KEY_SELECT) {
                curr--;
                backspace();
            }
            if (c) {
                *curr = c;
                curr++;
                iprintf("%c", c);
            }
            VBlankIntrWait();
        }

        free(inst_str);
    }
}
