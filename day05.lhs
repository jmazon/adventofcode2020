---
title:       "AoC day 5: Binary Boarding"
author:      "Jean-Baptiste Mazon"
date:        "2020-12-05T18:26:26+01:00"
updated:     "2021-01-01T15:54:26+01:00"
tags:        ["advent of code", aoc2020, haskell]
description: "Projecting to binary with the Prelude"
image:       aoc-2020-05.jpeg
---

The trick [today][aoc] is to squint and see binary coding for what it
is, and decode it as such instead of getting lost in implementation.
This post is [literate Haskell][lit], and one of the rare where there
is *no* header of imports and extensions.

[aoc]: https://adventofcode.com/2020/day/05
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day05.lhs

Seats are numbered from front to back, then left to right,[^order] so
the decoding for instructions is as follows.

[^order]: Had they been ordered right to left instead, the seat order
would have been the exact reverse of the lexicographical ASCII order.
Missed opportunity!

> toBit :: Char -> Int
> toBit 'F' = 0
> toBit 'B' = 1
> toBit 'L' = 0
> toBit 'R' = 1

I can then combine them all to a single seat number.

> passToSeat :: String -> Int
> passToSeat = foldl1 (\a b -> 2*a + b) . map toBit

And apply it to all tickets in input to find the maximum.

> main = do
>   passes <- lines <$> readFile "day05.in"
>   let seatIds = map passToSeat passes
>   print $ maximum seatIds

For part 2, the most reliable way to proceed without doing it by hand
is to simply transcribe the statement: I'm looking for a seat number
that's missing with its two neighbors[^seat] being present.

[^seat]: Of course they're not really neighbors if they span across
rows.

>   let seatRange = [minimum seatIds..maximum seatIds]
>   print $ filter (\s -> s + 1 `elem` seatIds
>                         && s - 1 `elem` seatIds
>                         && s `notElem` seatIds) seatRange

This concludes today's solution.  See you soon!
