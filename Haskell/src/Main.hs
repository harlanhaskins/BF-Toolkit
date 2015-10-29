{-# LANGUAGE RankNTypes #-}

module Main where

import Brainfuck.Core
import Brainfuck.Emitter
import qualified Data.Vector.Unboxed as V
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
emitTarget t        _  = "Invalid target: " ++ t 

main = do
    (Config filename passes t) <- execParser (info config fullDesc)
    program <- readFile filename
    let instructions = (optimizeN passes . compile) program
    let target = fmap (map toLower) t
    case target of
        (Just t') -> putStrLn $ emitTarget t' instructions
        Nothing -> do
            run (Brainfuck 0 (V.replicate 30000 0)) instructions
            return ()

