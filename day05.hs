passToSeat :: String -> Int
passToSeat = foldl1 (\a b -> 2*a + b) . map toBit
  where toBit 'F' = 0
        toBit 'B' = 1
        toBit 'L' = 0
        toBit 'R' = 1

main = do
  passes <- lines <$> readFile "in5.in"
  let seatIds = map passToSeat passes
  print $ maximum seatIds
  let seatRange = [minimum seatIds..maximum seatIds]
  print $ filter (\s -> s + 1 `elem` seatIds
                 && s - 1 `elem` seatIds
                 && s `notElem` seatIds) seatRange
