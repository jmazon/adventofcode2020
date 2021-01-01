---
title:       "AoC day 1: Report Repair"
author:      "Jean-Baptiste Mazon"
date:        "2020-12-01T06:09:00+01:00"
updated:     "2021-01-01T23:16:00+01:00"
tags:        ["advent of code", aoc2020, haskell]
description: "Finding an exact sum with list comprehensions"
image:       aoc-2020-01.jpeg
---

Opening the 2020 season, [Advent of Code day 1][aoc] presents the
exact sum problem: finding a pair or triplet of numbers from a list
that sum up to a specified number.  Namely, 2020.  This post is
[literate Haskell][lit], by the way.

[aoc]: https://adventofcode.com/2020/day/1
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day01.lhs

I'll parse using the standard functions from the Prelude, and find the
answers by “brute force” exhaustive search, the simplest idea that
could work.

> main :: IO ()
> main = do
>   ns <- map read . lines <$> readFile "day01.in"
>   print [ x*y   | x <- ns, y <- ns,          x+y   == 2020 ]
>   print [ x*y*z | x <- ns, y <- ns, z <- ns, x+y+z == 2020 ]

As you noticed, I avoid neither picking the same number multiple times
nor picking a set of numbers in a different order.  The puzzle input
guarantees the solution is unique anyway, so I can suffer the answers
being reported multiple times if it saves a bit of typing up front.

By an accident of life, I was actually up at six, so it was reasonable
to attempt speed.  As it turned out, I was kind of fast, but the site
was kind of down.  So no points for me.

Life's unfair.

Anyway, now the pressure is down, let's talk algorithms for a bit.
The solution presented above is $O(N^2)$ for part 1 and $O(N^3)$ for
part 2.  My input is 200 lines long, so both are still reasonable.
But what if it was longer?

The spirit of AoC is “you solve for your input”, so the canonical
answer to that is “your question doesn't make any sense.”  Thus the
rest of this post is purely theoretical.

An obvious improvement to the code above would be to actually avoid
all the duplication I mentioned.  The usual Haskelly way to do that is
to use the `tails` function from `Data.List`.

``` Haskell
[ x*y*z | x:xs <- tails ns, y:ys <- tails xs, z <- ys, x+y+z == 2020 ]
```

It's faster for sure.  But it's still $O(N^3)$, so not orders of
magnitude faster.  We can do better.

A common idea is to give those numbers some structure, so a single
number can be queried for being part of a sum that reaches the target.
Let's start with the pair sum problem.

* We could sort the numbers, for a cost of $O(N \log N)$ time.  The
  resulting sorted list could then be queried for a specific number in
  logarithmic time using binary search.  We could run over all the
  numbers and check if their complement to 2020 is in there too, for a
  total runtime of $O(N \log N)$ as well.
* With a sorted list, we could consume it from both ends: depending on
  the sign of $left+right-2020$, we know exactly which end to trim.
  This gives a cruise runtime of $O(N)$, but still a global one of
  $O(N \log N)$ since we have to sort first.
* We could… do nothing, for a cost of zero.  The resulting list could
  then be queried for a specific number in linear time using
  sequential search.  This gives a total runtime of $O(N^2)$.  This
  doesn't come as a surprise, as this is exactly my original code.
* We could hash the numbers, and query them in constant time.  This
  gives a theoretical[^theory] linear runtime of $O(N)$, the best we
  could possibly get!

[^theory]: Getting to proofs when it involves hashing is always a
complex matter.  See how I'm wisely dodging it here?

Of course I'm playing fast and loose with the “list” terminology: I'd
need constant-time access for the binary search to work fast.

How about the triplet sum then?

I could conceive of sorting, then for each number trying to find its
2020-complement as a sum of two numbers using the “both ends” method.
That'd be $O(N^2)$.  I never remember the optimal, remind me to go
check out the reddit thread.

In any case, this concludes today's solution.  See you soon!
