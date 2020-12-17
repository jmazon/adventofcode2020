---
title:       "AoC Day 17: Conway Cubes"
author:      "Jean-Baptiste Mazon"
date:        2020-12-17T09:59:38+01:00
tags:        ["advent of code", "aoc2020", "haskell"]
description: "Generically solving multiple game of life dimensionalities"
image:       aoc-2020-17.jpeg
---

[Another][prev] cellular automaton for [today's Advent of Code!][aoc]  Twice
in a year, aren't we the lucky ones!

[prev]: day16.html
[aoc]: https://adventofcode.com/2020/day/17

Here's some imports and language extensions to make the introduction
[spicy][lit].

[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day17.lhs

> {-# LANGUAGE ConstraintKinds #-}
> {-# LANGUAGE TypeApplications #-}
> import Prelude hiding ((.))
> import Control.Category
> import Control.Lens
> import Data.Foldable   (toList)
> import Data.Ix         (Ix,range)
> import Data.Set as Set (Set,singleton,fromList,filter,member)
> import Data.List       (elemIndices)
> import Linear

The sample is the classic 2D glider.  This hints at “shifting
borders”, so I'll represent the environment as a simple `Set` of
positions in case part 2 is about following it.

Parsing input can be coded as a simple composition of functions.

> parseInput = lines
>          >>> map (elemIndices '#')
>          >>> zipWith (\i js -> map (V2 i) js) [0..]
>          >>> concat
>          >>> fromList

What about the return type?  It's obviously a `Set (V2 Int)`.  But the
puzzle is in 3D, so I'll make it generic over the vector
dimensionality.  Well, not really, that would bring us into type-level
magic that's just not warranted here; I'll settle for making it
generic over the vector type.

> parseInput :: String -> Env V2
> type Env v = Set (v Int)

The automaton rule is the standard one, with no specific adjustment
for dimension.

> rule :: Bool -> Int -> Bool
> rule  True  n | n `notElem` [2,3] = False
> rule  False 3                     = True
> rule active _                     = active

Now to compute the neighborhood.  An easy way to do this is to use the
vectors' `Ix` instance, as I did on day 11.  But how am I to generate
the bounds?  I'll use the `Applicative` instance![^pointed]

[^pointed]: I don't really need Applicative, only Pointed.  But
Haskell doesn't separate them.

  > ``` { .repl }
  > λ> pure 42 :: V3 Int
  > V3 42 42 42
  > ```

> neighbors r = fromList
>   [ r ^+^ delta | delta <- range (pure (-1),pure 1), delta /= zero ]

What's the typing for this?  It needs quite a few constraints to work
right.

* The result is a `Set`.  This needs an `Ord` instance on the vector
  instance type.
* It adds (`^+^`) vectors.  This needs an `Additive` constraint on the
  vector type.
* It generates the unit “circle” using `range`.  This needs an `Ix`
  constraint on the vector instance type.
* It uses the `Applicative` trick for reasons detailed above.

That's a mouthful.  Let's package them for ease of reading.

> neighbors :: Conway v => v Int -> Env v
> type Conway v = (Ord (v Int),Additive v,Ix (v Int),Applicative v)

(This needs the `ConstraintKinds` extension.)

Now to implement the generational iteration.  With such a setup, it's
rather straightforward.

> step :: Conway v => Env v -> Env v
> step activeCubes = foldMap life (activeCubes <> fringe) where
>   fringe = foldMap neighbors activeCubes
>   life cube = case rule (isActive cube) (countNeighbors cube) of
>                 True  -> singleton cube
>                 False -> mempty
>   isActive cube = cube `member` activeCubes
>   countNeighbors = length . Set.filter isActive . neighbors

And…  that's it!  All we have to do now is run a few iterations on the
sample.

  > ``` { .repl }
  > λ> mapM_ print $ Data.List.take 5 $ iterate step sample
  > fromList [V2 0 1,V2 1 2,V2 2 0,V2 2 1,V2 2 2]
  > fromList [V2 1 0,V2 1 2,V2 2 1,V2 2 2,V2 3 1]
  > fromList [V2 1 2,V2 2 0,V2 2 2,V2 3 1,V2 3 2]
  > fromList [V2 1 1,V2 2 2,V2 2 3,V2 3 1,V2 3 2]
  > fromList [V2 1 2,V2 2 3,V2 3 1,V2 3 2,V2 3 3]
  > ```

It's not too visual, but comparing the first and last line you can
verify that our little glider has successfully shifted by `V2 1 1`.

But wait!  We're supposed to operate in 3D!

Well, if my generic style of implementing it worked, it should all be
a matter of using the code from a 3D starting point instead of the
flat one provided as input.  Let's write a function to convert the
input from 2D to any higher dimensionality.

> upgrade :: (R2 v,Conway v) => Env V2 -> Env v
> upgrade = fromList . map ((zero &) . set _xy) . toList

The `linear` package kindly provides the `R2`[^d2] class to access
2D subsets of vectors, so the conversion simply copies the 2D
components of the input to the two first coordinates of the receiving
higher-dimensional null vector.

[^d2]: Bleep!  Bloop!

And we can now solve part 1.

> main :: IO ()
> main = do
>   input <- parseInput <$> readFile "sample17.in"
>   print $ length $ iterate step (upgrade @V3 input) !! 6

Part 2 asks us to do the same thing in 4D.

Ok.

>   print $ length $ iterate step (upgrade @V4 input) !! 6

This concludes this day's solution.  I hope you enjoyed it, and see
you soon for a subsequent instalment!
