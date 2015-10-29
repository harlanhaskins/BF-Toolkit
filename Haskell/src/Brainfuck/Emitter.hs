module Brainfuck.Emitter where
import Data.List
import Brainfuck.Core

-- unlines adds a trailing newline which is undesired.
toLines = intercalate "\n"

class Emitter a where
    prologue :: Emitter a => a -> String
    handle   :: Emitter a => a -> Op -> (a, String)
    epilogue :: Emitter a => a -> String
    emit :: Emitter a => a -> [Op] -> String
    emit e ops = toLines . filter (/= "") $ [prologue e, (toLines . snd) $ mapAccumL handle e ops, epilogue e]

signString i = (if (i > 0) then "+" else "-")
signedShow i = signString i ++ ((show . abs) i)
inPlaceAssignment v = signString v ++ "= " ++ ((show . abs) v)

data IREmitter = IREmitter String
instance Emitter IREmitter where
    prologue _ = ""
    handle e@(IREmitter i) (ModPtr val) = (e, i ++ "Pointer " ++ signedShow val)
    handle e@(IREmitter i) (ModVal val) = (e, i ++ "Value " ++ signedShow val)
    handle e@(IREmitter i) Input        = (e, i ++ "Input")
    handle e@(IREmitter i) Output       = (e, i ++ "Output")
    handle e@(IREmitter i) Clear        = (e, i ++ "Clear")
    handle e@(IREmitter i) (Loop lops)  = let (_, handled) = mapAccumL handle (IREmitter (i ++ "    ")) lops
                                          in (e, toLines [i ++ "Loop: ", toLines handled])
    epilogue _ = ""

data CEmitter = CEmitter Int String
instance Emitter CEmitter where
    prologue (CEmitter s i) = toLines $
                          [ "#include <stdio.h>"
                          , "#include <string.h>"
                          , "char mem[" ++ (show s) ++ "];"
                          , "char *p = mem;"
                          , "int main() {"
                          , i ++ "memset(p, 0, " ++ (show s) ++ " * sizeof(char));"
                          ]
    handle e@(CEmitter _ i) (ModPtr v)   = (e, i ++ "p "  ++ inPlaceAssignment v ++ ";")
    handle e@(CEmitter _ i) (ModVal v)   = (e, i ++ "*p " ++ inPlaceAssignment v ++ ";")
    handle e@(CEmitter s i) (Loop ops)   = let (_, handled) = mapAccumL handle (CEmitter s (i ++ "    ")) ops
                                           in (e, toLines [i ++ "while (*p) {", toLines handled, i ++ "}"])
    handle e@(CEmitter _ i) Input        = (e, i ++ "*p = getchar();")
    handle e@(CEmitter _ i) Output       = (e, i ++ "putchar(*p);")
    handle e@(CEmitter _ i) Clear        = (e, i ++ "*p = 0;")
    epilogue _            = "}"

data JavaEmitter = JavaEmitter Int String
instance Emitter JavaEmitter where
    prologue (JavaEmitter s i) = toLines $
                                 [ "import java.io.IOException;"
                                 , "class BrainfuckProgram {"
                                 , "    public static void main(String args[]) throws IOException {"
                                 , "        char[] memory = new char[" ++ show s ++ "];"
                                 , "        int p = 0;"
                                 ]
    handle e@(JavaEmitter _ i) (ModPtr v)   = (e, i ++ "p "  ++ inPlaceAssignment v ++ ";")
    handle e@(JavaEmitter _ i) (ModVal v)   = (e, i ++ "memory[p] " ++ inPlaceAssignment v ++ ";")
    handle e@(JavaEmitter s i) (Loop ops)   = let (_, handled) = mapAccumL handle (JavaEmitter s (i ++ "    ")) ops
                                              in (e, toLines [i ++ "while ((int)memory[p] != 0) {", toLines handled, i ++ "}"])
    handle e@(JavaEmitter _ i) Input        = (e, i ++ "memory[p] = (char)System.in.read();")
    handle e@(JavaEmitter _ i) Output       = (e, i ++ "System.out.write(memory[p]);")
    handle e@(JavaEmitter _ i) Clear        = (e, i ++ "memory[p] = (char)0;")
    epilogue _                              = toLines [ "    }", "}" ]

data SwiftEmitter = SwiftEmitter Int String
instance Emitter SwiftEmitter where
    prologue (SwiftEmitter s i) = toLines $
                                  [ "import func Darwin.getchar"
                                  , "var p = 0"
                                  , "var memory = Array<UInt8>(count: " ++ show s ++ ", repeatedValue: 0)"
                                  ]
    handle e@(SwiftEmitter _ i) (ModPtr v)   = (e, i ++ "p = p &"  ++ signString v ++ " " ++ (show . abs) v)
    handle e@(SwiftEmitter _ i) (ModVal v)   = (e, i ++ "memory[p] = memory[p] &"  ++ signString v ++ " " ++ (show . abs) v)
    handle e@(SwiftEmitter s i) (Loop ops)   = let (_, handled) = mapAccumL handle (SwiftEmitter s (i ++ "    ")) ops
                                               in (e, toLines [i ++ "while memory[p] != 0 {", toLines handled, i ++ "}"])
    handle e@(SwiftEmitter _ i) Input        = (e, i ++ "memory[p] = String(getchar()).utf8.first!")
    handle e@(SwiftEmitter _ i) Output       = (e, i ++ "print(Character(UnicodeScalar(memory[p])), terminator: \"\")")
    handle e@(SwiftEmitter _ i) Clear        = (e, i ++ "memory[p] = 0")
    epilogue _                               = ""

data PythonEmitter = PythonEmitter Int String
instance Emitter PythonEmitter where
    prologue (PythonEmitter s _) = unlines
                                 [ "from __future__ import print_function"
                                 , "import sys"
                                 , "memory = [0] * " ++ show s
                                 , "p = 0"
                                 ]
    handle e@(PythonEmitter _ i) (ModVal v) = (e, i ++ "memory[p] " ++ inPlaceAssignment v)
    handle e@(PythonEmitter _ i) (ModPtr v) = (e, i ++ "p "         ++ inPlaceAssignment v)
    handle e@(PythonEmitter _ i) Input      = (e, i ++ "memory[p] = ord(sys.stdin.read(1))")
    handle e@(PythonEmitter _ i) Output     = (e, i ++ "print(chr(memory[p]), end='')")
    handle e@(PythonEmitter _ i) Clear      = (e, i ++ "memory[p] = 0")
    handle e@(PythonEmitter s i) (Loop ops) = let (_, handled) = mapAccumL handle (PythonEmitter s (i ++ "    ")) ops
                                              in (e, toLines [i ++ "while memory[p]:", toLines handled])
    epilogue _ = ""

data X86Emitter = X86Emitter Int Int
instance Emitter X86Emitter where
    prologue _ = toLines $
               [ "        global _main"
               , "        bits 64"
               , "        default rel"
               , "        section .text"
               , "_main:"
               , "        mov     r8, array"
               ]
    handle e (ModPtr v)                 = (e, "        " ++ (if v > 0 then "add" else "sub") ++ "     r8, " ++ ((show . abs) v))
    handle e (ModVal v)                 = (e, "        " ++ (if v > 0 then "add" else "sub") ++ "     byte [r8], " ++ ((show . abs) v))
    handle e Clear                      = (e, "        mov     byte [r8], 0")
    handle (X86Emitter s lc) (Loop ops) = let (e', handled) = mapAccumL handle (X86Emitter s (lc + 1)) ops
                                          in
                                          (e', toLines
                                               [ "loop_" ++ (show lc) ++ ":"
                                               , "        cmp     byte [r8], 0"
                                               , "        je      loop_" ++ (show lc) ++ "_end"
                                               , toLines handled
                                               , "        cmp     byte [r8], 0"
                                               , "        jne     loop_" ++ (show lc)
                                               , "loop_" ++ (show lc) ++ "_end:"
                                               ])
    handle e Input                      = (e, toLines
                                              [ "        mov    rax, 0x2000003"
                                              , "        mov    rdi, 0"
                                              , "        mov    rsi, r8"
                                              , "        mov    rdx, 1"
                                              , "        syscall"
                                              , "        mov    byte [r8], al"
                                              ])
    handle e Output                     = (e, toLines
                                              [ "        mov     rax, 0x2000004"
                                              , "        mov     rdi, 1"
                                              , "        mov     rsi, r8"
                                              , "        mov     rdx, 1"
                                              , "        syscall"
                                              ])
    epilogue (X86Emitter s _) = toLines $
                            [ "        mov     eax, 0x2000001"
                            , "        xor     rdi, rdi"
                            , "        syscall"
                            , "        section .data"
                            , "array:"
                            , "        times " ++ (show s) ++ " db 0"
                            ]

data HasmEmitter = HasmEmitter Int
instance Emitter HasmEmitter where
    prologue _ = ""
    handle e (ModPtr v)                = (e,  "        " ++ (if v > 0 then "add" else "sub") ++ " s0 s0 " ++ ((show . abs) v))
    handle e (ModVal v)                = (e,  toLines
                                              [ "        ld t0 s0"
                                              , "        " ++ (if v > 0 then "add" else "sub") ++ " t0 t0 " ++ ((show . abs) v)
                                              , "        str t0 s0"
                                              ])
    handle e Clear                     = (e,  toLines
                                              [ "        mov t0 0"
                                              , "        str t0 s0"
                                              ])
    handle (HasmEmitter lc) (Loop ops) = let (e', handled) = mapAccumL handle (HasmEmitter (lc + 1)) ops
                                         in
                                         (e', toLines
                                              [ "loop_" ++ (show lc) ++ ":"
                                              , "        ld t0 s0"
                                              , "        beqz t0 loop_" ++ show lc ++ "_end"
                                              , toLines handled
                                              , "        ld t0 s0"
                                              , "        bnez t0 loop_" ++ show lc
                                              , "loop_" ++ (show lc) ++ "_end:"
                                              ])
    handle e Input                     = (e,  toLines
                                              [ "        syscall 1"
                                              , "        str a0 s0"
                                              ])
    handle e Output                    = (e,  toLines
                                              [ "        ld a0 s0"
                                              , "        syscall 2"
                                              ])
    epilogue _ = ""

data MIPSEmitter = MIPSEmitter Int Int
instance Emitter MIPSEmitter where
    prologue (MIPSEmitter s _) = unlines
                               [ "       .data"
                               , "       .align 1"
                               , "array:"
                               , "       .space " ++ show s
                               , ""
                               , "       .text"
                               , "       .align 2"
                               , ""
                               , "       .globl main"
                               , "main:"
                               , "       la     $s0, array"
                               , "       addi   $sp, $sp, -8"
                               , "       sw     $ra, 0($sp)"
                               ]
    handle e (ModPtr v)                = (e,  "        addi    $s0, $s0, " ++ show v)
    handle e (ModVal v)                = (e,  toLines
                                              [ "        lb     $t0, 0($s0)"
                                              , "        addi   $t0, $t0, " ++ show v
                                              , "        sb     $t0, 0($s0)"
                                              ])
    handle e Clear                     = (e,  "        sb     $zero, 0($s0)")
    handle (MIPSEmitter s lc) (Loop ops) = let (e', handled) = mapAccumL handle (MIPSEmitter s (lc + 1)) ops
                                         in
                                         (e', toLines
                                              [ "loop_" ++ (show lc) ++ ":"
                                              , "        lb    $t0, 0($s0)"
                                              , "        beq   $t0, $zero, loop_" ++ show lc ++ "_end"
                                              , toLines handled
                                              , "        lb    $t0, 0($s0)"
                                              , "        bne   $t0, $zero, loop_" ++ show lc
                                              , "loop_" ++ (show lc) ++ "_end:"
                                              ])
    handle e Input                     = (e,  toLines
                                              [ "       li    $v0, 12"
                                              , "       syscall"
                                              , "       sb    $v0, 0($s0)"
                                              ])
    handle e Output                    = (e,  toLines
                                              [ "       lb     $t0, 0($s0)"
                                              , "       move   $a0, $t0"
                                              , "       li     $v0, 11"
                                              , "       syscall"
                                              ])

    epilogue _ = toLines
                 [ "       lw     $ra, 0($sp)"
                 , "       jr     $ra"
                 ]
