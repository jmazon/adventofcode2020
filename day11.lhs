---
title:       "AoC day 11: Seating System"
author:      "Jean-Baptiste Mazon"
date:        "2020-12-11T10:50:16+01:00"
tags:        ["advent of code", aoc2020, haskell, "cellular automaton"]
description: "Building a cellular automaton out of STRefs"
image:       aoc-haskell.jpeg
---

[AoC day 11][aoc11] is the day of the [cellular automaton][ca].

[aoc11]: https://adventofcode.com/2020/day/11
[ca]: https://en.wikipedia.org/wiki/Cellular_automaton

This post is mostly [literate Haskell][self], except I have no idea
what I'm doing.[^literate] So… let's see how this goes!

[self]: https://github.com/jmazon/adventofcode2020/blob/master/day11.lhs

As is customary, here's five pages of language extensions and imports
that you may freely skip over.  I simply haven't found the proper way
to conceal them yet.  Not that I've tried too hard.  Yet.

[^literate]: I have no idea what I'm doing as far as the literacy
processing goes.  My Haskell ought to be fine.

> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE ViewPatterns #-}
> import           Control.Monad
> import           Control.Monad.ST
> import           Data.Array
> import           Data.Function
> import           Data.Maybe
> import qualified Data.Map.Strict as Map
> import           Data.STRef
> import           Linear.V2

Ok, we're on!

The summarized topic of the day is to take a snapshot of a cellular
automaton after it converges.  The automaton ruleset is the same
between both parts; what changes is the topology.

Let's start by converting the input format to something tangible.
I'll simply represent the input as a two-dimensional array of
booleans, telling me whether there's a seat there.  Array dimensions
are taken from the number of input lines and the width of the first
one.  I won't do any involved error detection as the input is unique
and most likely correct.

> type Pos = V2 Int
> type SeatMap = Array Pos Bool
>
> -- | Safe seat map indexing.
> isSeat :: SeatMap -> Pos -> Bool
> isSeat seatMap pos | inRange (bounds seatMap) pos = seatMap ! pos
>                    | otherwise = False
>
> parse :: String -> SeatMap
> parse (lines -> input) = listArray (V2 1 1,V2 h w) (map readSeat (concat input))
>   where w = length (head input)
>         h = length input
>         readSeat '.' = False
>         readSeat 'L' = True

I want iteration to be fast, as there are going to be multiple of
them.  So I'll represent the environment as a mesh of `STRef`s, with
nodes mapping to seats.  Each node will hold the following
information:

* an occupied flag.  It will change from a generation to the next, so
  I'll make it a reference.
* links to the other seats involved in its update.  The information I
  need is the number of occupied ones.  So I can afford to skip the
  neighboring nodes and link directly to the `STRef`s within.

> data Node s = Node { nodeRef          ::  STRef s Bool
>                    , nodeNeighborRefs :: [STRef s Bool] }

With this node type, my environment state is simply a collection of
them.  A collection that doesn't really need any structure at all, so
I'll just use a list by default.

> type Env s = [Node s]

With this set up, I'm ready to write the generation iteration.

This specific cellular automaton doesn't distinguish among neighbors,
so all I need to make available to the rule function is the number of
active neighbors.

> type Rule = Bool -> Int -> Bool

On this year's automaton, I expect the stabilization to manifest as
an absence of change from a generation to the next.  An easy way to
make that apparent in our “mutable reference mesh” model is to split
the traditional update in two steps:

1) serialize the changes that occur in this generation
2) carry them out

This allows me to peek in-between and verify whether or not there are
actually any changes remaining.  It has the added advantage of not
touching the rule's input generation before all of the changes have
been computed, which avoids *that* class of bug.  (The counterpoint is
the short-term memory consumed by the change log.)

> type Change s = ST s ()
>
> scanChanges :: Rule -> Env s -> ST s [Change s]
> scanChanges f = mapMaybeM scanNode
>   where scanNode Node {..} = do
>           cur <- readSTRef nodeRef
>           n <- length <$> filterM readSTRef nodeNeighborRefs
>           let new = f cur n
>           pure (writeSTRef nodeRef new <$ guard (new /= cur))
>
> applyChanges :: [Change s] -> ST s ()
> applyChanges = sequence_

I still need an actual rule to be able to run this.  Let's write one
for both parts 1 and 2.

> data Flavor = Part1 | Part2
>
> rule :: Flavor -> Rule
> rule _  False   0 = True
> rule f  True    n | Part1 <- f, n >= 4 = False
>                   | Part2 <- f, n >= 5 = False
> rule _ occupied _ = occupied

Oh.  There's one thing I forgot.  To run all of this, I also need a
starting environment.  That's where we will observe most of the
difference between parts 1 and 2.

When I first solved the problem, I deliberately didn't use any smart,
let alone sane datastructure to do so.  I used the much-maligned lazy
character list-based Haskell I/O as is, resulting in
worse-than-suboptimal, quadratic algorithms.  I didn't care because
the input size is small, and this is only be performed once
anyway.^[In effect, constructing the environment took longer than
iterating the cellular automaton.  Which, I suppose, is a tribute to
the success of the “make iteration fast” approach.]

It's kind of trivial to port it to arrays and not publish crazy bad
code on the interwebz, so I'll spare your eyes and improve performance
in one stone.

The structural difference between parts 1 and 2 is which ~~cells~~
seats qualify as neighbors when computing the rule.  In part 1, the
seat neighbors are those of the 8 neighboring places that have a seat
on then.  In part 2, the neighbors are the first seat encountered
when radiating from the seat at hand in the 8 cardinal directions, up
to one per direction.

> -- I'm too lazy to define a “Dir” type to be mostly the same thing.
> cardinals :: [Pos]
> cardinals = filter (/= 0) $ range (V2 (-1) (-1),V2 1 1)
> 
> neighbors :: Flavor -> SeatMap -> Pos -> [Pos]
> neighbors Part1 sm p = filter (isSeat sm) (map (p +) cardinals)
> neighbors Part2 sm p = concatMap (firstSeat . trim . ray) cardinals
>   where ray dir = map (\i -> p + fromIntegral i * dir) [1 :: Int ..]
>         trim = takeWhile (inRange (bounds sm))
>         firstSeat = take 1 . filter (sm !)

To construct our mesh, I'll tie the knot over an internal/temporary
map from position to `STRef`.  My live solution actually used [the
RecursiveDo extension][rdo], but this doesn't really win anything
meaningful, so I'll keep it simple here for further readability.

[rdo]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html

> environment :: (Pos -> [Pos]) -> [Pos] -> ST s (Env s)
> environment nbs seatPoss = do
>   pos2ref <- Map.fromList <$> mapM (\p -> (p,) <$> newSTRef False) seatPoss
>   pure $
>     map ( \(pos,nodeRef) ->
>             let nodeNeighborRefs = map (pos2ref Map.!) (nbs pos)
>             in Node{..} )
>         (Map.assocs pos2ref)

Now, what did the puzzle request as an output?  The number of occupied
seats.  That's rather straightforward to compute.

> hash :: Env s -> ST s Int
> hash = fmap length . filterM (\Node{nodeRef} -> readSTRef nodeRef)

And I can now package the complete chain!

> solve :: Flavor -> SeatMap -> Int
> solve flavor seatMap = runST $ do
>
>   -- list of seat positions
>   let seatPoss = filter (seatMap !) (indices seatMap)
>
>   -- the two parameters: neighboring rule and threshold
>   let nbs = neighbors flavor seatMap
>       r = rule flavor
>
>   env <- environment nbs seatPoss
>   fix $ \loop -> do
>     scanChanges (rule flavor) env >>= \case
>       []      -> hash env
>       changes -> applyChanges changes *> loop

The rest is just boilerplate…

> main :: IO ()
> main = do
>   seatMap <- parse <$> readFile "day11.in"
>   print $ solve Part1 seatMap
>   print $ solve Part2 seatMap

…and a small helper.

> mapMaybeM :: Applicative m => (a -> m (Maybe b)) -> [a] -> m [b]
> mapMaybeM f = fmap catMaybes . traverse f

To summarize: I took a bottom-up approach, building up my cellular
automaton environment as a mesh of `STRef`s, over which I computed and
serialized a list of changes per generation.

This concludes this day's solution.  I hope you learned something
along the way!
