{-# LANGUAGE RecursiveDo #-}

import Relude
import Control.Monad.ST
import Data.STRef
import Data.List (elemIndex,elemIndices,(!!))
import qualified Data.List as List (lines)

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (i,j) = [ (i',j') | i' <- [i-1..i+1], j' <- [j-1..j+1], (i',j') /= (i,j) ]

main :: IO ()
main = do
  seatmap <- List.lines <$> readFile "in11.in"
  let seatPoss = concat $ zipWith (map . (,)) [0..] (map (elemIndices 'L') seatmap)
  print $ length seatPoss
{-
  print $ runST $ do
    rec (cells :: [(STRef s Bool,[STRef s Bool])]) <- forM seatPoss $ \p -> do
          let ns = neighbors p
              is = findIndices (`elem` ns) seatPoss
              ps = map (fst . (cells !!)) is
          r <- newSTRef False
          pure (r,ps)
    iters <- newSTRef 0
    fix $ \loop -> do
      traceShowM =<< readSTRef iters
      modifySTRef' iters succ
      changes <- fmap catMaybes . forM cells $ \(c,ns) -> do
        taken <- readSTRef c
        takenNeighbors <- length <$> filterM readSTRef ns
        case (taken,takenNeighbors) of
          (False,0) -> pure $ Just (writeSTRef c True)
          (True,n) | n >= 4 -> pure $ Just (writeSTRef c False)
          _ -> pure Nothing
      if null changes
        then length <$> filterM (readSTRef . fst) cells
        else sequence_ changes *> loop
-}
  print $ runST $ do
    rec cells <- forM seatPoss $ \p -> do
          let nss = neighbors' p :: [[(Int,Int)]]
              firstSeatIndex :: [(Int,Int)] -> Maybe Int
              firstSeatIndex = asum . map (`elemIndex` seatPoss)
              is = mapMaybe firstSeatIndex nss
              ps = map (fst . (cells !!)) is
          r <- newSTRef False
          pure (r,ps)
    fix $ \loop -> do
      changes <- fmap catMaybes . forM cells $ \(c,ns) -> do
        taken <- readSTRef c
        takenNeighbors <- length <$> filterM readSTRef ns
        case (taken,takenNeighbors) of
          (False,0) -> pure $ Just (writeSTRef c True)
          (True,n) | n >= 5 -> pure $ Just (writeSTRef c False)
          _ -> pure Nothing
      if null changes
        then length <$> filterM (readSTRef . fst) cells
        else sequence_ changes *> loop

add :: (Int,Int) -> (Int,Int) -> (Int,Int)
add (a,b) (c,d) = (a+c,b+d)

neighbors' :: (Int,Int) -> [[(Int,Int)]]
neighbors' (i,j) = [ drop 1 $ takeWhile inRange $ iterate (add (di,dj)) (i,j) | di <- [-1..1], dj <- [-1..1], (di,dj) /= (0,0) ]

inRange :: (Int,Int) -> Bool
inRange (i,j) = i >= 0 && j >= 0 && i < 100 && j < 100
