---
title:       "AoC day 12: Rain Risk"
author:      "Jean-Baptiste Mazon"
date:        2020-12-12T10:47:03+01:00
updated:     2020-12-27T17:24:03+01:00
tags:        ["advent of code", "aoc2020", "haskell"]
description: "Imaginary navigation"
image:       aoc-2020-12.jpeg
---

[Today's puzzle][aoc] is one of those typical “same instructions,
differing interpretations” problems AoC is so good at.  This post is a
[literate Haskell][lit] program, let's get the imports out of the way.

[aoc]: https://adventofcode.com/2020/day/12
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day12.lhs

> import Data.Complex
> import Data.List

Parsing is dead easy.  Assuming line-based instructions, I just pass
the first character as-is and convert the number to a `Double`.

> parseInstruction :: String -> (Char,Double)
> parseInstruction (i:n) = (i,read n)

Why a double?  I'm going to use `Complex` numbers, and in Haskell they
only have a useful `Num` instance if the underlying type implements
`RealFloat`.  So `Double` it is, even if the coordinates are only
expected to take integer values.

> type C = Complex Double

In part 1, `NESW` mean to move the ship by the specified distance,
whereas the `LRF` series implement “[turtle graphics][turtle]”.  So my
step function maintains the ship's position and direction.

[turtle]: https://en.wikipedia.org/wiki/Turtle_graphics

> go1 :: (C,C) -> (Char,Double) -> (C,C)
> go1 (pos,hdg) (i,n) = case i of
>     'N' -> (pos + (0 :+ n)    ,hdg)
>     'S' -> (pos - (0 :+ n)    ,hdg)
>     'W' -> (pos - (n :+ 0)    ,hdg)
>     'E' -> (pos + (n :+ 0)    ,hdg)
>     'F' -> (pos + (n :+ 0)*hdg,hdg)
>     'L' -> (pos               ,hdg * (0 :+   1 )^a)
>     'R' -> (pos               ,hdg * (0 :+ (-1))^a)
>   where a | round n `mod` 90 == 0 = round n `div` 90

In part 2, a “waypoint” is introduced.  Its use looks more like like a
[speed vector][vel] than a [navigational waypoint][waypoint], but
let's play along.  Here the only way to move is the `F` instruction;
`NESW` adjust the vector by addition, `LR` by rotation.

[vel]: https://en.wikipedia.org/wiki/Velocity
[waypoint]: https://en.wikipedia.org/wiki/Waypoint

> go2 :: (C,C) -> (Char,Double) -> (C,C)
> go2 (pos,wpt) (i,n) = case i of
>     'N' -> (pos               ,wpt + (0 :+ n))
>     'S' -> (pos               ,wpt - (0 :+ n))
>     'W' -> (pos               ,wpt - (n :+ 0))
>     'E' -> (pos               ,wpt + (n :+ 0))
>     'L' -> (pos               ,wpt * (0 :+   1 )^a)
>     'R' -> (pos               ,wpt * (0 :+ (-1))^a)
>     'F' -> (pos + (n :+ 0)*wpt,wpt)
>   where a | round n `mod` 90 == 0 = round n `div` 90

All that's left is to fold the instructions, pairing the appropriate
`go` function with its starting vector.

> main :: IO ()
> main = do
>   instrs <- map parseInstruction . lines <$> readFile "day12.in"
>   let (destination1,_) = foldl' go1 (0 :+ 0, 1 :+ 0) instrs
>   print $ round (dist destination1)
>   let (destination2,_) = foldl' go2 (0 :+ 0,10 :+ 1) instrs
>   print $ round (dist destination2)

And measuring the distance to the origin by Manhattan distance,
because for some reason that's what makes sense during a storm.

> dist :: C -> Double
> dist (x :+ y) = abs x + abs y

This concludes today's solution.  See you soon!
