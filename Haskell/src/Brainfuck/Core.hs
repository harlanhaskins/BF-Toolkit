module Brainfuck.Core where
import Data.Word8 (Word8)
import Data.Char (chr, ord)
import Data.List
import qualified Data.Vector.Unboxed as V
import Control.Exception (catch)
import System.IO.Error

data Brainfuck = Brainfuck Int (V.Vector Word8) deriving (Show, Eq)
data Op = ModPtr Int | ModVal Int | Input | Output | Loop [Op] | Clear deriving (Show, Eq)
data CompileState = CompileState
                  { stateStack :: [Op]
                  , stateOps :: [Op]
                  } deriving (Show, Eq)

compile :: String -> Either String [Op]
compile ops
    | (length . stateOps) finalState == 0 = Left "No operators parsed."
    | (length . stateStack) finalState > 0 = Left "Imbalanced loops"
    | otherwise = (Right . stateOps) finalState
    where finalState = compile' ops (CompileState [] [])

compile' :: String -> CompileState -> CompileState
compile' [] c                                             = c
compile' ('+':ops) c                                      = addOp (ModVal 1) ops c
compile' ('-':ops) c                                      = addOp (ModVal (-1)) ops c
compile' ('>':ops) c                                      = addOp (ModPtr 1) ops c
compile' ('<':ops) c                                      = addOp (ModPtr (-1)) ops c
compile' ('[':ops) (CompileState s ops')                  = compile' ops (CompileState ((Loop []):s) ops')
compile' (']':ops) (CompileState ((Loop lops):sops) ops') = addOp (Loop (reverse lops)) ops (CompileState sops ops') -- loop operations are stacked in reverse
compile' ('.':ops) c                                      = addOp Output ops c
compile' (',':ops) c                                      = addOp Input ops c
compile' (_:ops)   c                                      = compile' ops c

addOp :: Op -> String -> CompileState -> CompileState
addOp op ops (CompileState ((Loop lops):sops) ops') = compile' ops (CompileState ((Loop (op:lops)):sops) ops')
addOp op ops c = let (CompileState s ops') = compile' ops c in (CompileState s (op:ops'))

optimizeN :: Int -> [Op] -> [Op]
optimizeN = flip ((!!) . iterate optimize)

optimize []                           = []
optimize ((ModVal v):(ModVal v2):ops) = optimize $ (ModVal (v + v2)):ops
optimize ((ModPtr v):(ModPtr v2):ops) = optimize $ (ModPtr (v + v2)):ops
optimize ((Loop [ModVal _]):ops)      = Clear:optimize ops
optimize ((Loop lops):ops)            = (Loop (optimize lops)):optimize ops
optimize (o:ops)                      = o:optimize ops

run b [] = return b
run (Brainfuck p mem) ((ModVal v):ops) = run (Brainfuck p (mem V.// [(p, (mem V.! p) + (fromIntegral v))])) ops
run (Brainfuck p mem) ((ModPtr v):ops) = run (Brainfuck (p + v) mem) ops
run b@(Brainfuck p mem) ops@((Loop lops):ops')
    | mem V.! p == 0 = run b ops'
    | otherwise      = run b lops >>= (flip run) ops
run (Brainfuck p mem) (Input:ops) = do
    c <- getChar `catch` \e -> if isEOFError e then return '\0' else ioError e
    run (Brainfuck p (mem V.// [(p, (fromIntegral . ord) c)])) ops
run b@(Brainfuck p mem) (Output:ops) = do
    putChar $ (chr . fromIntegral) (mem V.! p)
    run b ops
run (Brainfuck p mem) (Clear:ops) = run (Brainfuck p (mem V.// [(p, 0)])) ops
