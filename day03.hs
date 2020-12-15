import Data.List.Split

main = do
  geology <- lines <$> readFile "in3.in"
  let used = geology
  let slope r d = length $ filter id $ zipWith (\l i -> l !! (i `mod` length l) == '#') (map head (chunksOf d used)) [0,r..]
  print $ slope 3 1
  print $ product $ [ slope 1 1, slope 3 1, slope 5 1, slope 7 1, slope 1 2 ]
