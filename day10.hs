{-# LANGUAGE TupleSections #-}
import Data.Function
import Data.List
import Control.Arrow

combinations :: Int -> [(Int,Int)] -> [(Int,Int)]
combinations to [] = [(to,1)]
combinations from xs = (:xs) $ (from,) $ sum $ map snd $ takeWhile ok xs
  where ok (to,_) = to - from <= 3

part2 :: [Int] -> Int
part2 = snd . head . foldr combinations []

main = do
  joltages <- map read. lines <$> readFile "in10.in"
  -- sanity: no dupes
  let [(1,ones),(3,threes)] = (0 : joltages) &
        sort &
        (zipWith (-) =<< tail) &
        sort & group &
        map (head &&& length)
  print $ ones * (threes + 1)
  print $ part2 (0:sort joltages)
