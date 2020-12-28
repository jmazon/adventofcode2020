---
title:       "AoC day 10: Adapter Array"
author:      "Jean-Baptiste Mazon"
date:        "2020-12-10T10:13:48+01:00"
updated:     "2020-12-28T00:18:56+01:00"
tags:        ["advent of code", aoc2020, haskell]
description: "Joltage surge by way of sorting and dynamic programming"
image:       aoc-2020-10.jpeg
---

The [challenge today][aoc] is a nice opportunity to go Bird-style with
code simplification.  So let's try it out, with Bird-tracked [literate
Haskell][lit] to boot.

[aoc]: https://adventofcode.com/2020/day/10
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day10.lhs

> {-# LANGUAGE RecordWildCards #-}
> import Control.Arrow ((&&&))
> import Data.Function ((&))
> import Data.List     (sort,group,foldl')

Part 1 asks to compute the sum of joltage differentials each adapter
would be subject to if we chained all of them from lowest to highest
between the outlet and the device.

I'll use the most underrated algorithm in computer science: sorting.

> part1 :: [Int] -> Int
> part1 ratings = ones * (threes + 1) where
>   rs = sort ratings
>   [(1,ones),(3,threes)] =
>     zipWith (-) rs (0 : rs) &
>     sort & group            &
>     map (head &&& length)

Processing after sorting is, from top to bottom: pairwise subtraction,
grouping by difference, tallying.  The difference between the outlet
and the first adapter is given by `zipWith`'s first returned element;
the difference between the last adapter and the device is always $3$
and is hard-coded as the $+1$ in the result.

You may notice I'm not only ignoring `2`s, but actually actively
pattern-matching against them.  I happen not to have any in my input.

Part 2 is noticeably trickier.

Instead of restricting the number of way the adapters are chained to a
single arrangement, we're counting all of them!  And there are *a
lot*.

How could I count them?  The natural way is to enumerate them with
pattern-matching and recursion.

``` Haskell
combinations :: [Int] -> [[Int]]
combinations [x] = [[x]]
combinations (j1:j2:js)
  | j2 - j1 <= 0 = error "bad joltage ordering"
  | j2 - j1 > 3 = mempty
  | otherwise   = takeJ2 <> skipJ2
  where takeJ2 = (j1 :) <$> combinations (j2:js)
        skipJ2 = guard (not (null js)) *> combinations (j1:js)
```

In plain English: the `combinations` function maps a (sorted) joltage
list to the number of ways to combine the matching adapters.  It
proceeds recursively:

* if there's only one joltage (at all, or remaining by recursion),
  there's exactly one way to combine it: just using it.
* if the gap between the smallest two joltages is greater than $3$,
  we're not going to be able to complete the chain at all, so the
  number of combinations is $0$.
* otherwise, there's two possibilities: we either use the second
  smallest joltage adapter or we don't.  Therefore the number of
  combinations is the sum of the number of combinations under both
  hypotheses.
* the null `guard` clause is there to forbid skipping the last
  adapter: it's the only valid link to the device, it has to be in.

For non-empty input (which mine is), it's easy to convince oneself
this will indeed terminate: there's a base case, and each recursive
call shortens the input list by one.

Let's actually try it on the first example.

  > ``` { .repl }
  > λ> let chains1 = combinations (0 : sort sample1)
  > λ> mapM_ print chains1
  > [0,1,4,5,6,7,10,11,12,15,16,19]
  > [0,1,4,5,6,7,10,12,15,16,19]
  > [0,1,4,5,7,10,11,12,15,16,19]
  > [0,1,4,5,7,10,12,15,16,19]
  > [0,1,4,6,7,10,11,12,15,16,19]
  > [0,1,4,6,7,10,12,15,16,19]
  > [0,1,4,7,10,11,12,15,16,19]
  > [0,1,4,7,10,12,15,16,19]
  > λ> length chains1
  > 8
  > ```

So far, so good.  How about the second example?

  > ``` { .repl }
  > λ> length (combinations (0 : sort sample2))
  > 19208
  > ```

I'm skipping actually printing them all because it's starting to get a
bit long, but I'll assume they're all good.

Unfortunately, it won't go much further with this approach.  The
reason is quite simple: at each recursion point, we have a maximal
branching factor of two.  In broad strokes, this means the total
running time for $N$ adapters could be the double of the running time
for $N-1$ adapters.  This is an exponential algorithm, it will only
make sense to use it for very small $N$s.

Let's refine it.

Since the result set is going to be too big to enumerate, I'll first
convert the algorithm to count the arrangements instead.

``` Haskell
combinations :: [Int] -> Int
combinations [_] = 1
combinations (j1:j2:js)
  | j2 - j1 > 3 = 0
  | otherwise   = takeJ2 + skipJ2
  where takeJ2 = combinations (j2:js)
        skipJ2 = if null js then 0 else combinations (j1:js)
```

I dropped the error checking because I now trust myself enough not to
call it with unsorted joltages.  I can verify it yields the same results:

  > ``` { .repl }
  > λ> combinations (0 : sort sample1)
  > 8
  > λ> combinations (0 : sort sample2)
  > 19208
  > ```

It still takes too long, though.  Consider that $19208$ result was
constructed by summing only ones and zeros.

So how do I make it faster?  The key observation is that it's
performing the same work multiple times.  It's not directly apparent
with this wording of the definition, but notice that the `takeJ2`
recursive call is actually a call on the function argument's tail.

Let's rewrite, avoiding to cons any list that isn't a tail of the
original input.

``` Haskell
combinations :: [Int] -> Int
combinations (h:t) = go h t where
  go _ [] = 1
  go from (to:js)
    | to - from > 3 = 0
    | otherwise     = takeIt + skipIt
    where takeIt = go to js
          skipIt = if null js then 0 else go from js
```

There's still nothing reusable, for now it's just a different
presentation of the function's calling convention.  But I can lift the
guard clause up to the recursive callsite by factoring the calls.

``` Haskell
combinations :: [Int] -> Int
combinations (h:t) = go h t where
  go _ [] = 1
  go from js = sum [ go to js'
                     | (to:js') <- tails js
                     , to - from <= 3 ]
```

This still works, but it's worse than the original: it traverses the
list of tails entirely for each position, whereas the previous
implementation short-circuited as soon as the gap got too big.  So
I'm somewhere between $O(3^N)$ and $O(N^N)$.  But this is fixable.

``` Haskell
combinations :: [Int] -> Int
combinations (h:t) = go h t where
  go _ [] = 1
  go from js = sum $ whileMaybe recurseIfReachable $ tails js where
    recurseIfReachable (to:js') = go to js' <$ guard (to - from <= 3)
    recurseIfReachable [] = Nothing
```

(`whileMaybe` is a variant of `mapMaybe` that short-circuits on first
`Nothing`.)

``` Haskell
whileMaybe :: (a -> Maybe b) -> [a] -> [b]
whileMaybe f = foldr (\a b -> maybe [] (: b) (f a)) []
```

Ok, I'm back on track.  To optimize this some more, I'll observe the
recursive call is performed on the tails of the current sublist.  This
is the wasteful operation.  Now's the time to perform the actual
[dynamic programming][dp] tranformation.

[dp]: https://en.wikipedia.org/wiki/Dynamic_programming

``` Haskell
combinations :: [Int] -> Int
combinations = fst . head . foldr go [] where
  go to [] = [(1,[to])]
  go from rjs@((_,js):_) = (sum (whileMaybe lookupIfReachable rjs),from:js) : rjs where
    lookupIfReachable (n,(to:_)) = n <$ guard (to - from <= 3)
    lookupIfReachable (_,[]) = Nothing
```

This was a mechanical conversion.  The former return value of the
recursion, `n`, is returned as the head of the result of the fold.
References to it now point to its place in the fold's right-hand side.
The fold constructs the entire list of results, since they're needed
to compute the result at each point.  I needed the tails too, so
they're returned and maintained there as well, as the second part of a
pair.

This implementation is $O(N)$ time, space and stack.  It's enough to
pass today's challenge.

Not too satisfying, though.  It's a bit ridiculous to maintain the
list's entire tail at each point, for one.  Remembering the joltage
there ought to be enough information.

``` Haskell
combinations :: [Int] -> Int
combinations = fst . head . foldr go [] where
  go to [] = [(1,to)]
  go from rjs = (sum (whileMaybe lookupIfReachable rjs),from) : rjs where
    lookupIfReachable (n,to) = n <$ guard (to - from <= 3)
```

Much more readable.  Now let's tackle performance.

The quickest win is stack.  For now, the computation is constructed as
a chain of dependencies from the first element to the last, from which
it “bounces” back, constructing the results and tails on the way back.
This is because the conversion stems from the recursive calls, who
also presented this two-way traverse pattern.

But the computation in itself is symmetrical.  Let's rewrite it as a
left fold.

``` Haskell
combinations :: [Int] -> Int
combinations = fst . head . foldl' go [] where
  go [] from = [(1,from)]
  go rjs to = (sum (whileMaybe lookupIfReaching rjs),to) : rjs where
    lookupIfReaching (n,from) = n <$ guard (to - from <= 3)
```

This was mechanical as well: flip the arguments to `go` and swap all
occurrences of `to` and `from`.

Let's pretend[^seq] the `foldl'` strictness extends to both members of
the pair: the algorithm is now constant stack, $O(N)$ time and space.

[^seq]: It's not that adding a `seq` call is hard, it just makes the
code less readable when we're going to trash it anyway.

Can we do better?  A bit, yes.

The final observation is that since the joltages are distinct and
ordered, the `whileMaybe` call will never need to peek at more than 3
of them for the `guard` to remain valid.  So instead of constructing
the entire list of values as I go, I can restrict it to three
elements.

> data Combinations = C
>   { twoBack :: !Int
>   , oneBack :: !Int
>   , current :: !Int
>   , joltage :: !Int
>   }

I'm shoehorning the joltage in so I can get rid of the pair and
benefit from the record's strict fields.

> combinations :: [Int] -> Int
> combinations = current . foldl' go (C 0 0 1 0) where
>   go C{..} to = case to - joltage of
>     1 -> C oneBack current (twoBack + oneBack + current) to
>     2 -> C current    0              (oneBack + current) to
>     3 -> C    0       0                         current  to

(Note that providing the initial $0$ joltage for the outlet is no
longer needed: it's a part of the initial `Combinations` record.)

Ta-da!  The code is now constant stack and space, still linear time.
It seems unlikely it's possible to do better than linear time if we
want the result to depend on the input.

It's debatable whether or not this constitutes an actual improvement:
there's quite a bit more number shuffling going around whereas the
previous implementation was closer to very straightforward consing.
The cost of latent RAM usage is to be pondered against the increased
reliance on the optimizer.

As always for all things optimization: measure!

> part2 :: [Int] -> Int
> part2 = combinations . sort
> 
> main :: IO ()
> main = do
>   joltages <- map read. lines <$> readFile "day10.in"
>   print $ part1 joltages
>   print $ part2 joltages

This concludes today's solution and improvements.  See you soon!
