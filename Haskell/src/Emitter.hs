module Emitter where
import Data.List
import Brainfuck

class Emitter a where
    prologue :: Emitter a => a -> String
    handle   :: Emitter a => a -> Op -> String
    epilogue :: Emitter a => a -> String
    emit :: Emitter a => a -> [Op] -> String
    emit e ops = unlines $ (prologue e):(map (handle e) ops ++ [epilogue e])

signString i = (if (i > 0) then "+" else "-")
signedShow i = signString i ++ ((show . abs) i)

data IREmitter = IREmitter
instance Emitter IREmitter where
    prologue _ = ""
    handle   _ = handleIR ""
    epilogue _ = ""

handleIR i (ModPtr val) = i ++ "Pointer " ++ signedShow val
handleIR i (ModVal val) = i ++ "Value " ++ signedShow val
handleIR i Input        = i ++ "Input"
handleIR i Output       = i ++ "Output"
handleIR i Clear        = i ++ "Clear"
handleIR i (Loop lops)  = i ++ "Loop: \n" ++ (unlines . map (handleIR (i ++ "    "))) lops

data CEmitter = CEmitter Int
instance Emitter CEmitter where
    prologue (CEmitter s) = unlines $
                          [ "#include <stdio.h>"
                          , "char mem[" ++ (show s) ++ "];"
                          , "char *p = mem;"
                          , "int main() {"
                          ]
    handle _ (ModPtr v)   = "p "  ++ (signString v) ++ "= " ++ ((show . abs) v) ++ ";"
    handle _ (ModVal v)   = "*p " ++ (signString v) ++ "= " ++ ((show . abs) v) ++ ";"
    handle e (Loop ops)   = unlines $ ["while (*p) {"] ++ (map (handle e) ops) ++ ["}"]
    handle _ Input        = "*p = getchar();"
    handle _ Output       = "putchar(*p);"
    handle _ Clear        = "*p = 0;"
    epilogue _            = "}"

data X86Emitter = X86Emitter Int Int
instance Emitter X86Emitter where
    prologue _ = unlines $
               [ "        global _main"
               , "        bits 64"
               , "        default rel"
               , "        section .text"
               , "_main:"
               , "        mov     r8, array"
               ]
    handle _ _ = ""
    epilogue (X86Emitter s _) = unlines $
                            [ "        mov     eax, 0x2000001"
                            , "        xor     rdi, rdi"
                            , "        syscall"
                            , "        section .data"
                            , "array:"
                            , "        times " ++ (show s) ++ " db 0"
                            ]
    emit e@(X86Emitter s l) ops = unlines $ (prologue e):((snd . mapAccumL handleX86 e) ops ++ [epilogue e])


handleX86 e (ModPtr v)                 = (e, "        " ++ (if v > 0 then "add" else "sub") ++ "     r8, " ++ ((show . abs) v))
handleX86 e (ModVal v)                 = (e, "        " ++ (if v > 0 then "add" else "sub") ++ "     byte [r8], " ++ ((show . abs) v))
handleX86 e Clear                      = (e, "        mov     byte [r8], 0")
handleX86 (X86Emitter s lc) (Loop ops) = let (e', handled) = mapAccumL handleX86 (X86Emitter s (lc + 1)) ops
                                         in
                                         (e', unlines
                                              [ "loop_" ++ (show lc) ++ ":"
                                              , "        cmp     byte [r8], 0"
                                              , "        je      loop_" ++ (show lc) ++ "_end"
                                              , unlines handled
                                              , "        cmp     byte [r8], 0"
                                              , "        jne     loop_" ++ (show lc)
                                              , "loop_" ++ (show lc) ++ "_end:"
                                              ])
handleX86 e Input                      = (e, unlines
                                             [ "        mov    rax, 0x2000003"
                                             , "        mov    rdi, 0"
                                             , "        mov    rsi, r8"
                                             , "        mov    rdx, 1"
                                             , "        syscall"
                                             , "        mov    byte [r8], al"
                                             ])
handleX86 e Output                     = (e, unlines
                                             [ "        mov     rax, 0x2000004"
                                             , "        mov     rdi, 1"
                                             , "        mov     rsi, r8"
                                             , "        mov     rdx, 1"
                                             , "        syscall"
                                             ])

data HasmEmitter = HasmEmitter Int
instance Emitter HasmEmitter where
    prologue _ = ""
    handle _ _ = ""
    epilogue _ = ""
    emit e = unlines . snd . mapAccumL handleHasm e

handleHasm e (ModPtr v)                = (e,  "        " ++ (if v > 0 then "add" else "sub") ++ " s0 s0 " ++ ((show . abs) v))
handleHasm e (ModVal v)                = (e,  unlines
                                              [ "        ld t0 s0"
                                              , "        " ++ (if v > 0 then "add" else "sub") ++ " t0 t0 " ++ ((show . abs) v)
                                              , "        str t0 s0"
                                              ])
handleHasm e Clear                     = (e,  unlines
                                              [ "        mov t0 0"
                                              , "        str t0 s0"
                                              ])
handleHasm (HasmEmitter lc) (Loop ops) = let (e', handled) = mapAccumL handleHasm (HasmEmitter (lc + 1)) ops
                                         in
                                         (e', unlines
                                              [ "loop_" ++ (show lc) ++ ":"
                                              , "        ld t0 s0"
                                              , "        beqz t0 loop_" ++ show lc ++ "_end"
                                              , unlines handled
                                              , "        ld t0 s0"
                                              , "        bnez t0 loop_" ++ show lc
                                              , "loop_" ++ (show lc) ++ "_end:"
                                              ])
handleHasm e Input                     = (e,  unlines
                                              [ "        syscall 1"
                                              , "        str a0 s0"
                                              ])
handleHasm e Output                    = (e,  unlines
                                              [ "        ld a0 s0"
                                              , "        syscall 2"
                                              ])
