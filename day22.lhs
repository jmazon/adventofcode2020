---
title:       "AoC day 22: Crab Combat"
author:      Jean-Baptiste Mazon
date:        2020-12-22T21:50:46+01:00
updated:     2020-12-25T22:06:06+01:00
tags:        ["advent of code", "aoc2020", "haskell"]
description: "Functions that call functions that call the same function again"
image:       aoc-2020-22.jpeg
---

Not much to talk about in [today's Advent of Code challenge][aoc]: I
just read the statement and implemented as it went.  As usual, this
post is a [literate Haskell][lit] program.

[aoc]: https://adventofcode.com/2020/day/22
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day22.lhs

> import           Data.List.Split
> import qualified Data.Set as Set

Space cards have a semi-infinite rank, no suit and no duplicates.
I'll represent a deck of them as a simple list.

> type Deck = [Int]

Reading the input would probably be easier with Emacs shortcuts, but
heck this is a literate Haskell program, I might just as well code it
up.

> parseInput :: String -> (Deck,Deck)
> parseInput i = (read <$> d1,read <$> d2)
>   where ["Player 1:":d1,"Player 2:":d2] = linesBy null (lines i)

Ok, let's tackle the actual problem.  The winner's score is computed
as a weighted sum, implemented with `Prelude` functions.

> score :: [Int] -> Int
> score s = sum $ zipWith (*) s [n,n-1..]
>   where n = length s

  > ``` { .repl }
  > λ> score [3,2,10,6,8,5,9,4,7,1]
  > 306
  > ```

Deciding which player wins a round is the determining aspect of the
day's two parts.  I'll extract that as a type and function.

> data Side = P1 | P2 deriving Show
> type Rule = Deck -> Deck -> Side
> 
> simpleGameRule :: Rule
> simpleGameRule (h1:_) (h2:_) = case compare h1 h2 of
>   LT -> P2
>   GT -> P1

Just as the statement says: winner is the one who shows the
highest-ranked card.  We take note that this function would be partial
if the input could contain duplicates.

Now to implement the full game logic.

> game :: Rule -> Deck -> Deck -> (Side,Int)
> game rule = go Set.empty where
>   go cl      d1         d2 | (d1,d2) `Set.member` cl = (P1,score d1)
>   go  _      []         d2    = (P2,score d2)
>   go  _      d1         []    = (P1,score d1)
>   go cl d1@(h1:t1) d2@(h2:t2) = case rule d1 d2 of
>       P1 -> go cl' (t1 ++ [h1,h2]) t2
>       P2 -> go cl' t1 (t2 ++ [h2,h1])
>     where cl' = Set.insert (d1,d2) cl

Pretty self-describing.  I've included the part 2 amendment for game
loop avoidance[^cl] that's unneeded in part 1, but doesn't hurt
either.  It's a bit unfortunate that the same full pattern-matching is
needed for both the distinctive round winner decision and for moving
the cards to their new place, but it'll have to do.

[^cl]: I call the accumulator `cl` for “closed” as a matter of habit
from writing graph search functions with an “open” and a “closed” node
set.

This solves part 1 flawlessly.

  > ``` { .repl }
  > λ> uncurry (game simpleGameRule) $ parseInput sample
  > (P2,306)
  > ```

Part 2 is simply a matter of transcribing the loop detection above,
and the new, recursive game rule.

> recursiveGameRule :: Rule
> recursiveGameRule d1@(h1:t1) d2@(h2:t2)
>   | length t1 < h1 || length t2 < h2 = simpleGameRule d1 d2
>   | otherwise = fst $ game recursiveGameRule (take h1 t1) (take h2 t2)

This solves part 2.

  > ``` { .repl }
  > λ> uncurry (game recursiveeGameRule) $ parseInput sample
  > (P2,291)
  > ```

Here's `main` for completeness.

> main :: IO ()
> main = do
>   (d1,d2) <- parseInput <$> readFile "day22.in"
>   print $ game simpleGameRule d1 d2
>   print $ game recursiveGameRule d1 d2

This concludes today's puzzle solution.
