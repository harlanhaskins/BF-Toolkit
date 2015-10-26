        add s0 s0 1
        ld t0 s0
        add t0 t0 10
        str t0 s0

        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

loop_0:
        ld t0 s0
        beqz t0 loop_0_end
loop_1:
        ld t0 s0
        beqz t0 loop_1_end
        ld t0 s0
        add t0 t0 5
        str t0 s0

loop_2:
        ld t0 s0
        beqz t0 loop_2_end
        add s0 s0 1
        ld t0 s0
        add t0 t0 8
        str t0 s0

        sub s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0


        ld t0 s0
        bnez t0 loop_2
loop_2_end:

        add s0 s0 1
        ld a0 s0
        syscall 2

        sub s0 s0 1
        ld t0 s0
        add t0 t0 6
        str t0 s0

loop_3:
        ld t0 s0
        beqz t0 loop_3_end
        add s0 s0 1
        ld t0 s0
        sub t0 t0 8
        str t0 s0

        sub s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0


        ld t0 s0
        bnez t0 loop_3
loop_3_end:

        ld t0 s0
        add t0 t0 1
        str t0 s0

        sub s0 s0 3

        ld t0 s0
        bnez t0 loop_1
loop_1_end:

        add s0 s0 1
        ld a0 s0
        syscall 2

        add s0 s0 2
loop_4:
        ld t0 s0
        beqz t0 loop_4_end
mov t0 0
str t0 s0

        sub s0 s0 1
loop_5:
        ld t0 s0
        beqz t0 loop_5_end
        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        sub s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0


        ld t0 s0
        bnez t0 loop_5
loop_5_end:

        add s0 s0 2
loop_6:
        ld t0 s0
        beqz t0 loop_6_end
        sub s0 s0 2
        ld t0 s0
        add t0 t0 1
        str t0 s0

        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        add s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0


        ld t0 s0
        bnez t0 loop_6
loop_6_end:

        sub s0 s0 1
loop_7:
        ld t0 s0
        beqz t0 loop_7_end
        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        sub s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0

loop_8:
        ld t0 s0
        beqz t0 loop_8_end
        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        sub s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0

loop_9:
        ld t0 s0
        beqz t0 loop_9_end
        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        sub s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0

loop_10:
        ld t0 s0
        beqz t0 loop_10_end
        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        sub s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0

loop_11:
        ld t0 s0
        beqz t0 loop_11_end
        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        sub s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0

loop_12:
        ld t0 s0
        beqz t0 loop_12_end
        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        sub s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0

loop_13:
        ld t0 s0
        beqz t0 loop_13_end
        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        sub s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0

loop_14:
        ld t0 s0
        beqz t0 loop_14_end
        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        sub s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0

loop_15:
        ld t0 s0
        beqz t0 loop_15_end
        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        sub s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0

loop_16:
        ld t0 s0
        beqz t0 loop_16_end
        add s0 s0 1
mov t0 0
str t0 s0

        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        sub s0 s0 3
        ld t0 s0
        sub t0 t0 1
        str t0 s0

loop_17:
        ld t0 s0
        beqz t0 loop_17_end
        add s0 s0 1
        ld t0 s0
        add t0 t0 1
        str t0 s0

        sub s0 s0 1
        ld t0 s0
        sub t0 t0 1
        str t0 s0


        ld t0 s0
        bnez t0 loop_17
loop_17_end:


        ld t0 s0
        bnez t0 loop_16
loop_16_end:


        ld t0 s0
        bnez t0 loop_15
loop_15_end:


        ld t0 s0
        bnez t0 loop_14
loop_14_end:


        ld t0 s0
        bnez t0 loop_13
loop_13_end:


        ld t0 s0
        bnez t0 loop_12
loop_12_end:


        ld t0 s0
        bnez t0 loop_11
loop_11_end:


        ld t0 s0
        bnez t0 loop_10
loop_10_end:


        ld t0 s0
        bnez t0 loop_9
loop_9_end:


        ld t0 s0
        bnez t0 loop_8
loop_8_end:


        ld t0 s0
        bnez t0 loop_7
loop_7_end:

        ld t0 s0
        add t0 t0 1
        str t0 s0

        add s0 s0 3

        ld t0 s0
        bnez t0 loop_4
loop_4_end:

        sub s0 s0 3

        ld t0 s0
        bnez t0 loop_0
loop_0_end:
