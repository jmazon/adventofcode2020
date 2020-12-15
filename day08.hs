{-# LANGUAGE ViewPatterns #-}
import qualified Data.IntSet as S
import Control.Monad
import Data.Bool

main = do
  code <- map decode . lines <$> readFile "in8.in"
  print $ simulateToLoop code
  print $ (simulateToEnd code :: Maybe Int)

simulateToLoop is = go S.empty 0 0 where
  go cl ip a | ip `S.member` cl = a
             | i <- is !! ip = case i of
                 Nop _ -> go cl' (ip+1) a
                 Acc d -> go cl' (ip+1) (a+d)
                 Jmp d -> go cl' (ip+d) a
           where cl' = S.insert ip cl

simulateToEnd is = go S.empty True 0 0 where
  go cl ch ip a | ip `S.member` cl = mzero
                | ip == length is = pure a
                | i <- is !! ip = case i of
                    Nop d -> bool id (mplus $ go cl' False (ip+d) a) ch $ go cl' ch (ip+1) a
                    Acc d -> go cl' ch (ip+1) (a+d)
                    Jmp d -> bool id (mplus $ go cl' False (ip+1) a) ch $ go cl' ch (ip+d) a
           where cl' = S.insert ip cl

data Instruction = Nop Int | Acc Int | Jmp Int deriving Show
decode (words -> [operation,argument]) = case operation of
  "nop" -> Nop (readSigned argument)
  "acc" -> Acc (readSigned argument)
  "jmp" -> Jmp (readSigned argument)

readSigned ('+':n) = read n
readSigned n = read n
