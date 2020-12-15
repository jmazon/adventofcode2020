import Prelude
import Data.Char
import Data.Complex
import Data.List

parse :: String -> (String,Double)
parse w = (i,read n) where (i,n) = break isDigit w

main :: IO ()
main = do
  instrs <- map parse . lines <$> readFile "in12.in"
  let (dest1,_) = foldl' go (0 :+ 0,1 :+ 0) instrs
  print $ abs (realPart dest1) + abs (imagPart dest1)
  let (dest2,_) = foldl' go' (0 :+ 0,10 :+ 1) instrs
  print $ abs (realPart dest2) + abs (imagPart dest2)

go :: (Complex Double,Complex Double) -> (String,Double) -> (Complex Double,Complex Double)
go (pos,hdg) (i,n) = case i of
    "N" -> (pos + (0 :+ n),hdg)
    "S" -> (pos - (0 :+ n),hdg)
    "W" -> (pos - (n :+ 0),hdg)
    "E" -> (pos + (n :+ 0),hdg)
    "F" -> (pos + (n :+ 0)*hdg,hdg)
    "L" -> (pos,hdg * (0 :+ 1)^a)
    "R" -> (pos,hdg * (0 :+ (-1))^a)
  where a | round n `mod` 90 == 0 = round n `div` 90

go' :: (Complex Double,Complex Double) -> (String,Double) -> (Complex Double,Complex Double)
go' (pos,wpt) (i,n) = case i of
    "N" -> (pos,wpt + (0 :+ n))
    "S" -> (pos,wpt - (0 :+ n))
    "W" -> (pos,wpt - (n :+ 0))
    "E" -> (pos,wpt + (n :+ 0))
    "L" -> (pos,wpt * (0 :+ 1)^a)
    "R" -> (pos,wpt * (0 :+ (-1))^a)
    "F" -> (pos + (n :+ 0)*wpt,wpt)
  where a | round n `mod` 90 == 0 = round n `div` 90
