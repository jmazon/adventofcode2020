{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import Data.Foldable
import Data.List.Split
import Data.Sequence (Seq(Empty,(:<|)),(|>))
import qualified Data.Sequence as Q
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State.Strict

type Deck = Seq Int
data Side = P1 | P2 deriving (Show,Eq)

parseInput :: String -> (Deck,Deck)
parseInput i = (Q.fromList (read <$> p1),Q.fromList (read <$> p2))
  where ["Player 1:":p1,"Player 2:":p2] = linesBy null (lines i)

memo2 :: Ord a
  => (a -> a -> State (Map.Map (a,a) r) r)
  -> a -> a -> State (Map.Map (a,a) r) r
memo2 f a1 a2 = do
  let k = (a1,a2)
  gets (Map.lookup k) >>= \case
    Just res -> pure res
    Nothing -> do
      res <- f a1 a2
      modify' (Map.insert k res)
      pure res

type M = State (Map.Map (Deck,Deck) (Side,Int))
type Rule = Deck -> Deck -> M Side

game :: Rule -> Deck -> Deck -> M (Side,Int)
game rule = memo2 (go Set.empty) where
  go cl p1 p2 | (p1,p2) `Set.member` cl = pure (P1,conclude p1)
  go _ Empty p2 = pure (P2,conclude p2)
  go _  p1 Empty = pure (P1,conclude p1)
  go cl p1@(h1 :<| t1) p2@(h2 :<| t2) = do
    let cl' = Set.insert (p1,p2) cl
    rule p1 p2 >>= \case
      P1 -> go cl' (t1 |> h1 |> h2) t2
      P2 -> go cl' t1 (t2 |> h2 |> h1)

conclude :: Foldable t => t Int -> Int
conclude s = sum $ zipWith (*) (toList s) [n,n-1..]
  where n = length s

simpleGameRule :: Rule
simpleGameRule (h1 :<| _) (h2 :<| _) = case compare h1 h2 of
  LT -> pure P2
  GT -> pure P1

recursiveGameRule :: Rule
recursiveGameRule p1@(h1 :<| t1) p2@(h2 :<| t2)
  | Q.length t1 >= h1 && Q.length t2 >= h2
    = fst <$> game recursiveGameRule (Q.take h1 t1) (Q.take h2 t2)
  | otherwise = simpleGameRule p1 p2

main :: IO ()
main = do
  (p1,p2) <- parseInput <$> readFile "day22.in"
  print $ evalState (game simpleGameRule p1 p2) Map.empty
  print $ evalState (game recursiveGameRule p1 p2) Map.empty
