{-# LANGUAGE RankNTypes #-}

module Main where

import Brainfuck.Core
import Brainfuck.Emitter
import qualified Data.Vector.Unboxed as V
import Data.Either
import Data.Char
import Options.Applicative
import System.Environment

data Config = Config
            { filename :: String
            , optimizationPasses :: Int
            , target :: Maybe String
            } deriving Show

config = Config
     <$> argument str
         ( metavar "FILE" )
     <*> option auto
         ( long "optimization"
        <> short 'o'
        <> metavar "PASSES"
        <> value 0
        <> help "How many optimization passes to apply when compiling the Brainfuck" )
     <*> (optional . strOption)
         ( long "target"
        <> short 't'
        <> metavar "TARGET"
        <> help "The compile target." )

emitTarget "x86"    os = emit (X86Emitter 30000 0) os
emitTarget "python" os = emit (PythonEmitter 30000 "") os
emitTarget "c"      os = emit (CEmitter 30000 "    ") os
emitTarget "hasm"   os = emit (HasmEmitter 0) os
emitTarget "ir"     os = emit (IREmitter "") os
emitTarget "mips"     os = emit (MIPSEmitter 30000 0) os
emitTarget t        _  = "Invalid target: " ++ t

main = do
    (Config filename passes t) <- execParser (info config fullDesc)
    program <- readFile filename
    let result = compile program
    either (putStrLn . ("Error: " ++)) (handle t . optimizeN passes) result
    where handle t ops = case (fmap (map toLower) t) of
            (Just t') -> putStrLn $ emitTarget t' ops
            Nothing -> do
                run (Brainfuck 0 (V.replicate 30000 0)) ops
                return ()

