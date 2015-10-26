module Main where

import Brainfuck
import Emitter
import qualified Data.Vector.Unboxed as V
import System.Environment

main = do
    [filename] <- getArgs
    program <- readFile filename
    let instructions = (optimizeN 3 . compile) program
    putStrLn $ emit (HasmEmitter 0) instructions
    run (Brainfuck 0 (V.replicate 30000 0)) instructions
