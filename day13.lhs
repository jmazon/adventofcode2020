---
title:       "AoC day 13: Shuttle Search"
author:      "Jean-Baptiste Mazon"
date:        2020-12-13T12:34:16+01:00
updated:     2020-12-27T15:45:16+01:00
tags:        ["advent of code", "aoc2020", "haskell"]
description: "Catching the bus with chinese arithmetic"
image:       aoc-2020-13.jpeg
---

Various grades of modular arithmetic for [today's puzzle][aoc].
Let's start with the obligatory [literate Haskell][lit] header.

[aoc]: https://adventofcode.com/2020/day/13
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day13.lhs

> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE TupleSections #-}
> import Control.Arrow
> import Data.Function
> import Data.List
> import Data.Maybe
> import Data.Ord
> import Text.Read

The puzzle input is a starting time and a list of bus IDs presented in
an unusual manner.

> data Puzzle = Puzzle
>   { earliest :: Int
>   , busIds :: [Maybe Int]
>   }

With `Int`s being used both as timestamps and bus IDs, I'd typically
`newtype` them to avoid using one for the other by accident.  But the
bus IDs double as a time period.  Tough luck, it's all staying
undistinguished `Int`s and I'll just have to be careful.

I'll parse the CSV with `break` and `unfoldr`.

> parseInput :: String -> Puzzle
> parseInput input = Puzzle
>     { earliest = read start
>     , busIds   = unfoldr go schedule
>     }
>   where
>     [start,schedule] = lines input
>     go "" = Nothing
>     go s  = Just $ readMaybe *** drop 1 $ break (== ',') s

Part 1 asks for the first bus to depart the airport after my plane
lands.  The natural thing to do can't be beaten: check all busses for
the time to wait before their next departure.

> earliestBus :: Puzzle -> (Int,Int)
> earliestBus Puzzle{..} =
>   catMaybes busIds &
>   map (id &&& timeToNextDeparture earliest) &
>   minimumBy (comparing snd)

How do I compute the time to wait before a specific bus next departs?
Modular arithmetic!

Bus number $m$ departs every $m$ minutes.
$$ departure \equiv 0\ (\mod m) $$

The wait time is the time between the plane's arrival and the bus's
departure.
$$ wait = departure - arrival $$

Therefore, for a wait time between $0$ and $m-1$:
$$ wait \equiv -arrival\ (\mod m) $$

> timeToNextDeparture :: Int -> Int -> Int
> timeToNextDeparture arrival period = negate arrival `mod` period

In part 2, we are to find which arrival time would generate a wait
time for each bus ID equal tp its position in the input list.

That's a direct application of the [Chinese Remainder Theorem][crt].
When I need it I usually just copy-paste the algorithm from Wikipedia
or other source, but it so happens I actually understood it this time,
so I'll detail a bit more.

[crt]: https://en.wikipedia.org/wiki/Chinese_remainder_theorem

The theorem in itself just states a solution exists for some
conditions on the chosen moduli, “all being distinct primes” being a
strict subset.^[It's the case for mine, and probably yours too.]

The interesting part is generating a solution.  The idea is a close
parallel to [Lagrange's interpolation polynomials][lagrange]: we'll
use a linear basis among the moduli.  In other words, we'll generate a
solution as a linear combination of numbers whose residue is $1$ with
regard to a specific modulus and $0$ for all others.

[lagrange]: https://en.wikipedia.org/wiki/Lagrange_polynomial

How is such a number found?  It's $0$ for all moduli but one by virtue
of being a multiple of their product.  We want it to be $1$ for the
chosen special modulus.  That modulus and the product of all others
are coprime, so we can use [Bézout's theorem][bézout] to solve for it:
$$ \exists (u,v) \in \mathbb{Z}^2, u\Pi_i + v m_i = 1 $$

[bézout]: https://en.wikipedia.org/wiki/B%C3%A9zout%27s_identity

$u$ and $v$ are the result of the [extended Euclid's algorithm][euclid].

[euclid]: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm

> egcd :: Int -> Int -> (Int,Int)
> egcd 1 0 = (1,0)
> egcd _ 0 = error "egcd: Not coprime"
> egcd a b = let (u,v) = egcd b (a `mod` b)
>            in (v,u-a `div` b * v)

The construction for the CRT's solution then follows.  (Note how I
don't care about the Bézout coefficient for the product of all “other”
moduli.)

> chinese :: [(Int,Int)] -> Int
> chinese divRems = x `mod` π where
>   (divisors,remainders) = unzip divRems
>   π = product divisors
>   factors = map unitFor divisors
>   unitFor m = snd (egcd m π_i) * π_i where π_i = π `div` m
>   x = sum (zipWith (*) remainders factors)

The rest is a matter of mapping the input to fit.

> earliestArrival :: Puzzle -> Int
> earliestArrival Puzzle{busIds} =
>   chinese $ catMaybes $
>   zipWith (\bus wait -> (,negate wait) <$> bus) busIds [0..]

The `main` wrapper for completeness.

> main :: IO ()
> main = do
>   input <- parseInput <$> readFile "day13.in"
>   print $ uncurry (*) $ earliestBus input
>   print $ earliestArrival input

This concludes today's solution.  See you soon!
