//
//  BrainfuckEmitter.swift
//  BrainfuckCLI
//
//  Created by Harlan Haskins on 10/20/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

import Foundation
import Swift

protocol BrainfuckEmitter {
    var brainfuck: Brainfuck { get }
    init(brainfuck: Brainfuck)
    func setup() -> String?
    func handle(_ op: Operator) -> String
    func tearDown() -> String?
}

extension BrainfuckEmitter {
    func emit() -> String {
        return [
            setup(),
            brainfuck.instructions.map(self.handle).lines(),
            tearDown()
        ].flatMap { $0 }.lines()
    }
    func setup() ->  String? {
        return nil
    }
    func tearDown() -> String? {
        return nil
    }
}

class IREmitter: BrainfuckEmitter {
    let brainfuck: Brainfuck
    
    required init(brainfuck: Brainfuck) {
        self.brainfuck = brainfuck
    }
    
    var indentation = ""

    func handle(_ op: Operator) -> String {
        switch op {
        case .modifyPointer(let amount):
            let modifier = (amount < 0 ? "-" : "+")
            return "Pointer \(modifier)\(abs(amount))"
        case .modifyValue(let amount):
            let modifier = (amount < 0 ? "-" : "+")
            return "Value \(modifier)\(abs(amount))"
        case .loop(let ops):
            let first = "Loop:"
            indentation += "    "
            let body = ops.lazy.map(self.handle).map { self.indentation + $0  }.lines()
            indentation = indentation.substring(to: indentation.index(indentation.endIndex, offsetBy: -4))
            return [first, body].lines()
        case .input: return "Input"
        case .output: return "Output"
        case .clear: return "Clear"
        }
    }
}

struct CEmitter: BrainfuckEmitter {
    let brainfuck: Brainfuck
    
    func setup() -> String? {
        return [
            "#include <stdio.h>",
            "char mem[\(brainfuck.memorySpace.count)];",
            "char *p = mem;",
            "int main() {"
        ].lines()
    }
    
    func handle(_ op: Operator) -> String {
        switch op {
        case .input: return "*p = getchar();"
        case .output: return "putchar(*p);"
        case .loop(let ops):
            return [
                "while (*p) {",
                ops.map(self.handle).lines(),
                "}"
            ].lines()
        case .modifyPointer(let amount):
            let modifier = amount < 0 ? "-" : "+"
            return "p \(modifier)= \(abs(amount));"
        case .modifyValue(let amount):
            let modifier = amount < 0 ? "-" : "+"
            return "*p \(modifier)= \(abs(amount));"
        case .clear: return "*p = 0;"
        }
    }
    
    func tearDown() -> String? {
        return [
            "return 0;",
            "}"
        ].lines()
    }
}

struct SwiftEmitter: BrainfuckEmitter {
    let brainfuck: Brainfuck
    
    func handle(_ op: Operator) -> String {
        switch op {
        case .input: return "memory[p] = String(getchar()).utf8.first!"
        case .output: return "print(Character(UnicodeScalar(memory[p])), terminator: \"\")"
        case .modifyValue(let amount):
            let modifier = amount < 0 ? "-" : "+"
            return "memory[p] = memory[p] &\(modifier) \(abs(amount))"
        case .modifyPointer(let amount):
            let modifier = amount < 0 ? "-" : "+"
            return "p = p &\(modifier) \(abs(amount))"
        case .loop(let ops):
            return [
                "while memory[p] != 0 {",
                ops.map(self.handle).lines(),
                "}"
            ].lines()
        case .clear: return "memory[p] = 0"
        }
    }
    
    func setup() -> String? {
        return [
            "import func Darwin.getchar",
            "var p = 0",
            "var memory = Array<UInt8>(count: \(brainfuck.memorySpace.count), repeatedValue: 0)"
        ].lines()
    }
}

struct JavaEmitter: BrainfuckEmitter {
    let brainfuck: Brainfuck
    
    func setup() -> String? {
        return [
            "import java.io.IOException;",
            "class BrainfuckProgram {",
            "public static void main(String args[]) throws IOException {",
            "char[] memory = new char[\(brainfuck.memorySpace.count)];",
            "int p = 0;"
        ].lines()
    }
    
    func handle(_ op: Operator) -> String {
        switch op {
        case .modifyPointer(let amount):
            let modifier = amount < 0 ? "-" : "+"
            return "p \(modifier)= \(abs(amount));"
        case .modifyValue(let amount):
            let modifier = amount < 0 ? "-" : "+"
            return "memory[p] \(modifier)= \(abs(amount));"
        case .input: return "memory[p] = (char)System.in.read();"
        case .output: return "System.out.write(memory[p]);"
        case .loop(let ops):
            return [
                "while ((int)memory[p] != 0) {",
                ops.map(self.handle).lines(),
                "}"
            ].lines()
        case .clear: return "memory[p] = (char)0;"
        }
    }
    
    func tearDown() -> String? {
        return [
            "}",
            "}"
        ].lines()
    }
}

class PythonEmitter: BrainfuckEmitter {
    let brainfuck: Brainfuck
    var indentation = ""
    
    required init(brainfuck: Brainfuck) {
        self.brainfuck = brainfuck
    }
    
    func setup() -> String? {
        return [
            "from __future__ import print_function",
            "import sys",
            "memory = [0] * \(brainfuck.memorySpace.count)",
            "p = 0"
        ].lines()
    }
    
    func handle(_ op: Operator) -> String {
        switch op {
        case .modifyPointer(let amount):
            let modifier = amount < 0 ? "-" : "+"
            return "p \(modifier)= \(abs(amount))"
        case .modifyValue(let amount):
            let modifier = amount < 0 ? "-" : "+"
            return "memory[p] \(modifier)= \(abs(amount))"
        case .loop(let ops):
            let first = "while memory[p]:"
            indentation += "    "
            let body = ops.lazy.map(self.handle).map { self.indentation + $0  }.lines()
            indentation = indentation.substring(to: indentation.index(indentation.endIndex, offsetBy: -4))
            return [first, body].lines()
        case .input: return "memory[p] = ord(sys.stdin.read(1))"
        case .output: return "print(chr(memory[p]), end='')"
        case .clear: return "memory[p] = 0"
        }
    }
}

class X86Emitter: BrainfuckEmitter {
    let brainfuck: Brainfuck
    var loopCounter = 0
    
    required init(brainfuck: Brainfuck) {
        self.brainfuck = brainfuck
    }
    
    func setup() -> String? {
        return [
            "        global _main",
            "        bits 64",
            "        default rel",
            "        section .text",
            "_main:",
            "        mov     r8, array",
        ].lines()
    }
    
    func handle(_ op: Operator) -> String {
        switch op {
        case .modifyPointer(let amount):
            return "        add     r8, \(amount)"
        case .modifyValue(let amount):
            return "        add     byte [r8], \(amount)"
        case .loop(let ops):
            let firstSet = [
                "loop_\(loopCounter):",
                "        cmp     byte [r8], 0",
                "        je      loop_\(loopCounter)_end"
            ]
            let secondSet = [
                "        cmp     byte [r8], 0",
                "        jne     loop_\(loopCounter)",
                "loop_\(loopCounter)_end:"
            ]
            loopCounter += 1
            return (firstSet + ops.map(self.handle) + secondSet).lines()
        case .input:
            return [
                "        mov    rax, 0x2000003",
                "        mov    rdi, \(stdin.pointee._file)",
                "        mov    rsi, r8",
                "        mov    rdx, 1",
                "        syscall",
                "        mov    byte [r8], al",
            ].lines()
        case .output:
            return [
                "        mov     rax, 0x2000004",
                "        mov     rdi, \(stdout.pointee._file)",
                "        mov     rsi, r8",
                "        mov     rdx, 1",
                "        syscall",
            ].lines()
        case .clear:
            return "        mov     byte [r8], 0"
        }
    }
    
    func tearDown() -> String? {
        return [
            "        mov     eax, 0x2000001",
            "        xor     rdi, rdi",
            "        syscall",
            "        section .data",
            "array:",
            "        times \(brainfuck.memorySpace.count) db 0"
        ].lines()
    }
}

class MIPSEmitter: BrainfuckEmitter {
    let brainfuck: Brainfuck
    var loopCounter = 0
required     
    init(brainfuck: Brainfuck) {
        self.brainfuck = brainfuck
    }
    
    func setup() -> String? {
        return [
            "       .data",
            "       .align 1",
            "array:",
            "       .space \(brainfuck.memorySpace.count)",
            "",
            "       .text",
            "       .align 2",
            "",
            "       .globl main",
            "main:",
            "       la     $s0, array",
            "       addi   $sp, $sp, -8",
            "       sw     $ra, 0($sp)"
        ].lines()
    }
    
    func handle(_ op: Operator) -> String {
        switch op {
        case .modifyPointer(let amount):
            return "       addi   $s0, $s0, \(2 * amount)"
        case .modifyValue(let amount):
            return [
                "       lb     $t0, 0($s0)",
                "       addi   $t0, $t0, \(amount)",
                "       sb     $t0, 0($s0)"
            ].lines()
        case .loop(let ops):
            let firstSet = [
                "loop_\(loopCounter): ",
                "       lb     $t0, 0($s0)",
                "       beq    $t0, $zero, loop_\(loopCounter)_end"
            ]
            let secondSet = [
                "       j      loop_\(loopCounter)",
                "loop_\(loopCounter)_end:"
            ]
            loopCounter += 1
            return (firstSet + ops.map(self.handle) + secondSet).lines()
        case .output:
            return [
                "       lb     $t0, 0($s0)",
                "       move   $a0, $t0",
                "       li     $v0, 11",
                "       syscall"
            ].lines()
        case .input:
            return [
                "       li     $v0, 12",
                "       syscall",
                "       sb    $v0, 0($s0)"
            ].lines()
        case .clear:
            return "       sb   $zero, 0($s0)"
        }
    }
    
    func tearDown() -> String? {
        return [
            "       lw     $ra, 0($sp)",
            "       jr     $ra"
        ].lines()
    }
    
}

extension Sequence where Iterator.Element == String {
    func lines() -> String {
        return self.joined(separator: "\n")
    }
}
