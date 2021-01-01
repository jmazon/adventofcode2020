---
title:       "AoC day 6: Custom Customs"
author:      "Jean-Baptiste Mazon"
date:        "2020-12-06T13:09:09+01:00"
updated:     "2021-01-01T14:37:09+01:00"
tags:        ["advent of code", aoc2020, haskell]
description: "Assisting customs with set operations"
image:       aoc-2020-06.jpeg
---

[Today's puzzle][aoc] is to perform some queries on a database of
survey results.  This post is [literate Haskell][lit].^[For
consistency.  For a three-liner that could condense to one, it's not
buying much.]

[aoc]: https://adventofcode.com/2020/day/06
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day06.lhs

> import Data.List.Split
> import Data.List
> main = do

The data is provided as paragraphs of lines of characters,
representing groups of results of questions that were answered
positively.

I'll use the old Perl trick to split on `"\n\n"` to extract
paragraphs, then split on lines.

>   gs <- map lines . splitOn "\n\n" <$> readFile "day06.in"

Part 1 is the number of questions to which someone in the group
answered positively, summed across groups.  In set parlance, that's
the union of the positive answers.  `Data.List` happens to have
primitives for set operations.

>   print $ sum $ map (length . foldr1 union) gs

Part 2 is the number of questions to which everyone in the group
answered positively, summed across groups.  In set parlance, that's
the intersection of the positive answers.

>   print $ sum $ map (length . foldr1 intersect) gs

And that's all there is to it.  See you soon for another AoC solution!
