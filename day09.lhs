---
title:       "AoC day 9: Encoding Error"
author:      "Jean-Baptiste Mazon"
date:        "2020-12-09T10:38:18+01:00"
updated:     "2020-12-29T13:09:18+01:00"
tags:        ["advent of code", aoc2020, haskell]
description: "Finding meaning in lists of integers with zippers and scans"
image:       aoc-2020-09.jpeg
---

Lists of integers are at the core of [today's puzzle][aoc].  This post
is a [literate Haskell][lit] program; here's a few imports to skip
over.

[aoc]: https://adventofcode.com/2020/day/09
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day09.lhs

> {-# LANGUAGE NamedFieldPuns #-}
> import Control.Monad (guard)
> import Data.List     (find,tails)

Part 1 asks to identify the first number that cannot be expressed as
the sum of two distinct numbers taken among its 25 predecessors.

There's not much to fuss about, just doing it is sufficient.  I'll use
a list zipper.

> data Zip a = Zip { trail :: ![a], cursor :: !a, list :: [a] }

The zipper provides a view on an element of a list in context.  It can
be moved left or right in constant time/space.  For this problem I'll
only need going right.

> advance :: Zip a -> Zip a
> advance (Zip t c (c':l)) = Zip (c:t) c' l

I can convert from a list by simply expanding the cursor on the list's
first element.

> fromList :: [a] -> Zip a
> fromList (h:t) = Zip [] h t

A cursor position is valid if I can find a pair of distinct[^distinct]
numbers among the 25 previous that sum up to it.

[^distinct]: I didn't notice that distinctness constraint when I first
solved it.  And I didn't need it to solve.  YMMV.

> valid :: Zip Int -> Bool
> valid Zip{trail,cursor} = or $ do
>   (x:ys) <- tails (take 25 trail)
>   y <- ys
>   -- guard (x /= y)
>   pure $ cursor == x + y

Part 1 is then solved with a straightforward function composition.

> part1 :: [Int] -> Int
> part1 =
>   maybe (error "All numbers are valid.") cursor .
>   find (not . valid)                            .
>   drop 25                                       .
>   iterate advance                               .
>   fromList

Instead of a pair, part 2 asks to find a contiguous subsequence that
sums to the number found in part 1.  The classical response is to
construct a list of partial sums, so the sum can be searched for in
quadratic time as a difference of two elements.

> part2 :: [Int] -> Int -> [Int]
> part2 ns =
>   let partials = scanl (+) 0 ns
>   in \s -> head $ do
>     ((a,from):_:bts) <- tails (zip partials [0..])
>     (b,to) <- bts
>     guard (b - a == s)
>     pure $ take (to - from) $ drop from ns

Here's `main` for completeness.

> main = do
>   ns <- map read . lines <$> readFile "day09.in"
>   let special = part1 ns
>   print special
>   let contiguous = part2 ns special
>   print $ minimum contiguous + maximum contiguous

This concludes today's solution.  See you soon!
