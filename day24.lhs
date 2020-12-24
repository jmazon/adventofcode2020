---
title:       "AoC day 24: Lobby Layout"
author:      "Jean-Baptiste Mazon"
date:        2020-12-24T10:36:19+01:00
tags:        ["advent of code", "aoc2020", "haskell"]
description: "Implementing what's requested"
image:       aoc-2020-24.jpeg
---

I don't have a fancy summary for [today's challenge][aoc]: really, all
that had to be done was implement.  This post is a [literate
Haskell][lit] program.

[aoc]: https://adventofcode.com/2020/day/24
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day24.lhs

> {-# LANGUAGE LambdaCase #-}
> import           Data.Foldable (foldl')
> import           Data.Set      (Set,member,insert,delete)
> import qualified Data.Set as Set

The puzzle input is provided as concatenated hex-grid steps.  The
first thing to do is split them up for easier consumption.  Luckily
enough, they form a valid [prefix code][pfx], so I can greedily
process and decode them.

[pfx]: https://en.wikipedia.org/wiki/Prefix_code

> splitDirs :: String -> [Dir]
> splitDirs     []       =    []
> splitDirs ('e':    ds) = E  : splitDirs ds
> splitDirs ('s':'e':ds) = SE : splitDirs ds
> splitDirs ('s':'w':ds) = SW : splitDirs ds
> splitDirs ('w':    ds) = W  : splitDirs ds
> splitDirs ('n':'w':ds) = NW : splitDirs ds
> splitDirs ('n':'e':ds) = NE : splitDirs ds

Next I need to recognize which paths lead up to the same tile so I can
count it as flipped back instead of count both as flipped once.  There
are multiple ways to do this.  I just introduced a coordinate pair in
a system where they'd uniquely identify a tile.  ([the “odd-r”
one][oddr], except I order them row first)

[oddr]: https://www.redblobgames.com/grids/hexagons/#coordinates

> type Pos = (Int,Int)
> data Dir = E | SE | SW | W | NW | NE deriving (Enum,Bounded)

Now to write the walking function once and for all.  This is the most
likely place to trip.  Straight east/west movement is easy enough to
write.  The others… need a bit of thought.  Simple checks include:
north/south symmetry says the $j$ parameter must be the same between
`Nx` and `Sx`; east/west symmetry says `xE` should be one more than
`xW` on the $j$ dimension; the row offset rule says moving vertically
from an even row should result in one less than moving from an odd
row.

> walk :: Pos -> Dir -> Pos
> walk (i,j) = \case
>   E           -> (i  ,j+1)
>   SE | even i -> (i+1,j  )
>      | odd i  -> (i+1,j+1)
>   SW | even i -> (i+1,j-1)
>      | odd i  -> (i+1,j  )
>   W           -> (i  ,j-1)
>   NW | even i -> (i-1,j-1)
>      | odd i  -> (i-1,j  )
>   NE | even i -> (i-1,j  )
>      | odd i  -> (i-1,j+1)

With this out of the way, I can summarize a path as a position.

> type Path = [Dir]
> 
> pathToPos :: Path -> Pos
> pathToPos = foldl' walk (0,0)

This lets me maintain a set of flipped-to-black tiles.

> type TileSet = Set Pos
>
> flipTiles :: [Pos] -> TileSet
> flipTiles = foldl' xorInsert Set.empty

Using a small helper to flip a set element.[^stackage]

[^stackage]: I'd use [`alterF`][alterf], but `containers >= 0.6.3.1`
isn't on stackage yet :-(

[alterF]: https://hackage.haskell.org/package/containers/docs/Data-Set.html#v:alterF

> xorInsert :: Ord a => Set a -> a -> Set a
> xorInsert s e | member e s = delete e s
>               | otherwise  = insert e s

Surprise! Part 2 is a cellular automaton!

It doesn't really have anything specific going for it, I can re-use my
function from [day 17][day17] directly:

[day17]: /posts/day17.html

  > ``` Haskell
  > step :: Conway v => Env v -> Env v
  > step activeCubes = foldMap life (activeCubes <> fringe) where
  >   fringe = foldMap neighbors activeCubes
  >   life cube = case rule (isActive cube) (countNeighbors cube) of
  >                 True  -> singleton cube
  >                 False -> mempty
  >   isActive cube = cube `member` activeCubes
  >   countNeighbors = length . Set.filter isActive . neighbors
  > ```

Mmm… maybe I'll edit it *just a bit*.

> step :: TileSet -> TileSet
> step blackTiles = foldMap life (blackTiles <> fringe) where
>   fringe = foldMap neighbors blackTiles
>   life cube = case rule (isActive cube) (countNeighbors cube) of
>                 True  -> Set.singleton cube
>                 False -> mempty
>   isActive cube = cube `member` blackTiles
>   countNeighbors = length . Set.filter isActive . neighbors

Much better; I hope you didn't blink.  I'll still need to transcribe
the rule from the statement…

> rule :: Bool -> Int -> Bool
> rule True  = (`elem` [1,2])
> rule False = (== 2)

…and the neighborhood function,

> neighbors :: Pos -> Set Pos
> neighbors p = Set.fromList $ walk p <$> allDirs

…using the usual `universe` helper.

> allDirs :: [Dir]
> allDirs = [minBound..maxBound]

And… that's it!  Here's the `main` wrapper for completeness.

> main :: IO ()
> main = do
>   input <- map splitDirs . lines <$> readFile "day24.in"
>   let tiling = flipTiles $ pathToPos <$> input
>   print $ Set.size tiling
>   print $ Set.size $ iterate step tiling !! 100

This concludes today's solution.  I'm afraid there wasn't much to
learn from it, but at least I can demonstrate the pieces fit together
nicely.

See you soon!
