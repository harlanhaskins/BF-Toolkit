module Brainfuck where
import Data.Word8 (Word8)
import Data.Char (chr, ord)
import Data.List
import qualified Data.Vector.Unboxed as V
import Control.Exception (catch)
import System.IO.Error

data Brainfuck = Brainfuck Int (V.Vector Word8) deriving (Show, Eq)
data Op = ModPtr Int | ModVal Int | Input | Output | Loop [Op] | Clear deriving (Show, Eq)

compile = (flip compile') []

compile' []        _ = []
compile' ('+':ops) s = addOp (ModVal 1) ops s
compile' ('-':ops) s = addOp (ModVal (-1)) ops s
compile' ('>':ops) s = addOp (ModPtr 1) ops s
compile' ('<':ops) s = addOp (ModPtr (-1)) ops s
compile' ('[':ops) s = compile' ops ((Loop []):s)
compile' (']':ops) ((Loop lops):sops) = addOp (Loop (reverse lops)) ops sops -- loop operations are stacked in reverse
compile' ('.':ops) s = addOp Output ops s
compile' (',':ops) s = addOp Input ops s
compile' (_:ops)   s = compile' ops s

addOp op ops ((Loop lops):sops) = compile' ops ((Loop (op:lops)):sops)
addOp op ops s = op:compile' ops s

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
