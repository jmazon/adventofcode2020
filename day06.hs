import Data.List.Split
import Data.List
main = do
  gs <- map lines . splitOn "\n\n" <$> readFile "in6.in"
  print $ sum $ map (length . foldr1 union) gs
  print $ sum $ map (length . foldr1 intersect) gs

