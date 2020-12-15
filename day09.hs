{-# LANGUAGE ViewPatterns #-}
import Debug.Trace
import Control.Monad
import Data.List
import Data.Maybe

invalid (splitAt 25 -> (preamble,target:_))
  | null sums = Just target
  | otherwise = Nothing
  where sums = do
          (x:ys) <- tails preamble
          y <- ys
          guard $ x + y == target
          pure target

main = do
  ns <- map read . lines <$> readFile "in9.in"
  let special = head $ mapMaybe invalid $ tails ns
  print special
  let acc = scanl (+) 0 ns
  mapM_ print $ do
        es@((a,(lo,_)):_) <- tails (zip acc (zip (ns ++ repeat undefined) (undefined:ns)))
        ((b,(_,hi)),range) <- drop 2 (zip es (inits (fst . snd <$> es)))
        guard $ b - a == special
        traceShowM (a,lo,b,hi)
        traceShowM range
        pure (minimum range + maximum range)
