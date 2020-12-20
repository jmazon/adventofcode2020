---
title:       "AoC day 20: Jurassic Jigsaw"
author:      "Jean-Baptiste Mazon"
date:        2020-12-20T21:28:27+01:00
tags:        ["advent of code", "aoc2020", "haskell"]
description: "Flipping, rotating and collating bits"
image:       aoc-2020-20.jpeg
---

Bitmap raster operations are the name of the game for [today's
problem][aoc].  Please ignore my [literate Haskell][lit] imports so we
can jump right in.

[aoc]: https://adventofcode.com/2020/day/20
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day20.lhs

> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ViewPatterns #-}
> import           Control.Applicative (liftA2)
> import           Control.Lens        (view,_1,_2,(+~))
> import           Control.Monad       (guard)
> import           Data.Bool           (bool)
> import           Data.Char           (isDigit)
> import           Data.Function       ((&),on)
> import           Data.Ix             (Ix,range)
> import           Data.List           (delete,tails,transpose,groupBy)
> import           Data.List.Split     (linesBy)
> import           Data.Map.Strict     (Map,(!))
> import qualified Data.Map.Strict as Map
> import           Data.Set            (Set)
> import qualified Data.Set as Set     
> import           Linear              (V2(V2),zero)

My input file isn't too long—144 tiles.  I ought to be fine just using
lists as a data structure.

> type Image = [[Bool]]

The monochrome images come with an ID number.  To avoid mixing them
up, I'll pack them together.

> data Tile = Tile
>   { tileId    :: Int
>   , tileImage :: Image
>   }
>   deriving Eq

Now to actually parse the input.  I'll use the same trick as usual to
split paragraphs.

> parseInput :: String -> [Tile]
> parseInput = map parseTile . linesBy null . lines

To parse the `Tile` structure itself, I'll try something
experimental[^forme] to try and answer the great question: can view
patterns be nested?

[^forme]: Experimental *for me*.  I expect them to be fully specified.

> parseTile :: [String] -> Tile
> parseTile ((break (== ' ') ->
>               ("Tile",' ' : (span isDigit -> (read -> tileId,":"))))
>            : ((map . map) (== '#') -> tileImage))
>   = Tile{..}

…it seems like they can.  I wouldn't go as far as calling that a good
idea, though.  Ye gods this is horrendeous!  Until someone can show me
a visually-pleasing way to format that, I'll classify this as “never
again”.

Ok, so our image was split up in tiles, and the postman tripped on the
way so now they're a mess on the floor.  And since they're square and
transparent, there's no way to know which way to place them so all of
them align.

Let's formalize the various ways it could be messed up.

> data Setup = Setup
>   { sTranspose :: Bool
>   , sOuterFlip :: Bool
>   , sInnerFlip :: Bool
>   }
>   deriving (Eq,Ord,Ix,Bounded)

In real life I'd call the enumeration “4 right-angle rotations for
2 sides”.  But we're doing computer processing on lists, I'll adjust
my terminology.

For some reason GHC can't derive `Enum` on this structure, so I'll
define my “universe” enumerator using `Ix` instead.

> allSetups :: [Setup]
> allSetups = range (minBound,maxBound)

Now I can transform my tile to bring it to and from all of those
orientations.

> reorient :: Tile -> Setup -> Tile
> reorient tile Setup{..} = tile { tileImage = morph (tileImage tile) }
>   where morph = bool id transpose     sTranspose .
>                 bool id reverse       sOuterFlip .
>                 bool id (map reverse) sInnerFlip

I chose to keep it in the same type, as there's no information loss.
I need the ID number to remain with it if I want to track it back when
I'm done shifting bits around.

Now let's move towards reassembly.  The tiles are going to be arranged
in a grid, so integral-`V2`-based cartesian coordinates are fine.

> type Vec = V2 Int
> type Pos = Vec

On the other hand, trying all permutations of the tiles in a square
patterns seems a bit wasteful.  I'll likely get drastically better
performance starting with an arbitrary tile and accreting the others
on the fly.

But I can't know in advance where in the final square the starting
tile will end.  So I'll use a `Map` instead of a bidimensional array.

> type Assembly = Map Pos Tile

Today's neighborhood function only recognizes 4 neighbors per tile.
Please indulge me while I try to go creative with `linear`'s `V2` and
lenses.  I'm attempting to keep up the practice, here.[^lenses]

[^lenses]: I got back to lenses for day 14, but that part of the
journey isn't published yet.  I'm on it!

> neighbors :: Pos -> Set Pos
> neighbors = Set.fromList . sequence (liftA2 (+~) [_1,_2] [-1,1])

No, it's not shorter,[^shorter] more efficient or in any way better
than a simple list comprehension-based implementation.  But I welcomed
the fun.

[^shorter]: Well, ok, it *is* shorter, but the point would stand even
if it weren't.

I'd better check it actually works.

  > ``` { .repl }
  > λ> neighbors (V2 44 55)
  > fromList [V2 43 55,V2 44 54,V2 44 56,V2 45 55]
  > ```

Looks good :-)

Let's write a helper to determine whether two tiles can be placed next
to each other.

> compatible :: Tile -> Vec -> Tile -> Bool
> compatible t1 delta t2 = edge1 (tileImage t1) == edge2 (tileImage t2)
>   where
>     (edge1,edge2) = case delta of
>         V2 (-1) 0 -> (top,bottom)
>         V2   1  0 -> (bottom,top)
>         V2 0 (-1) -> (left,right)
>         V2 0   1  -> (right,left)
>     top    = head
>     bottom = last
>     left   = map head
>     right  = map last

So to reassemble the big square from the tiles, I'll place the tiles
one by one, next to each other, starting with an arbitrary one—the
first—, always ensuring the borders of the newly placed one are
compatible with the previously placed others.  Backtracking through
the list monad.

I expect there to be only one solution up to orientations, and the
input size is small, so I'm not throwing any optimization in.  I pick,
in order:

1. an empty spot with at least one neighboring spot filled
2. a loose tile
3. an orientation for it

If it's compatible with its neighbors, I recurse, else I backtrack.

> reassemble :: [Tile] -> Assembly
> reassemble (startingTile : tiles0) =
>     head $ go (Map.singleton zero startingTile) tiles0
>   where
>     go placed [] = pure placed
>     go placed tiles = do
>       pos <- Map.keysSet placed                  &
>              Set.map neighbors                   &
>              Set.unions                          &
>              Set.filter (`Map.notMember` placed) &
>              Set.toList
>       let nPoss = neighbors pos                    &
>                   Set.filter (`Map.member` placed) &
>                   Set.toList
> 
>       tile <- tiles
>       setup <- allSetups
>       let pl = reorient tile setup
> 
>       guard $ all (\nPos -> compatible pl (nPos - pos) (placed ! nPos)) nPoss
>       go (Map.insert pos pl placed) (delete tile tiles)

If all goes well, this returns a “floating” map of oriented tiles.
The following helper extracts the floating square's corner ID numbers,
for the part 1 answer field.

> cornerIds :: Assembly -> [Int]
> cornerIds arr = tileId . (arr !) <$>
>     [ V2 top left, V2 top right, V2 bottom left, V2 bottom right ]
>   where
>     ixs = Map.keys arr
>     top =    minimum (view _1 <$> ixs)
>     left =   minimum (view _2 <$> ixs)
>     bottom = maximum (view _1 <$> ixs)
>     right =  maximum (view _2 <$> ixs)

You may have noticed I'm using the generic `_1` and `_2` lenses
instead of `linear`'s `V2`-specialized `_x` and `_y`, here and for the
neighborhood function.  It's the clash of worlds: in a linear algebra
world, I'd normally consider X to be the first coordinate, horizontal
and going right, and Y the second, vertical going up.  In a raster
world[^matrices], I rather see the first coordinate going down and the
second right.  I chose to go with the raster vision all the way, and
evade that part of the confusion by using the numbered accessors.

[^matrices]: Matrix addressing, ironically, fits the raster vision.

  > ``` { .repl }
  > λ> let assembly = reassemble (parseInput sample)
  > λ> cornerIds assembly
  > [2971,1171,1951,3079]
  > ```

All good and a gold star!

For part 2 we merge the oriented tiles.  I'm somewhat arbitrarily
repacking it in a `Tile` so I can reuse the orientation routines
later.

> merge :: Assembly -> Tile
> merge =
>   Tile undefined                       . -- image to tile
>   foldr1 (++)                          . -- fuse vertically
>   map (foldr1 (zipWith (++)))          . -- fuse horizontally
>   (map . map) snd                      . -- drop coordinates
>   groupBy ((==) `on` (view (_1 . _1))) . -- rasterize
>   Map.assocs                           . -- map to list
>   fmap tileTrimmedImage                  -- trim

The tile trimmer is similar in style: composition of simpler
functions.

> tileTrimmedImage :: Tile -> Image
> tileTrimmedImage = trimRight . trimLeft . trimBottom . trimTop . tileImage
>   where
>     trimBottom = init
>     trimTop    = tail
>     trimRight  = map init
>     trimLeft   = map tail

Now to locate sea monsters.  Let's make sure we know what we're
looking for.

> seaMonster :: Image
> seaMonster = (map . map) (== '#')
>   [ "                  # "
>   , "#    ##    ##    ###"
>   , " #  #  #  #  #  #   "
>   ]

To count them, I'll match by following both lists of lists in
lockstep.  There are two pitfalls here.

1. I'm working on lists of lists of `Bool`s.  It's all too natural to
   reach for zip-like functions, *e.g.* `zipWith (==)` or similar.
   But those constructs stop on the shortest sequence of those
   provided.  When matching, it has a very different meaning to
   exhaust the pattern (success) or the input string (failure).  I'll
   alleviate with a dedicated helper.
2. The comparison semantics are asymmetrical.

> match :: Image -> Image -> Bool
> match ref input = allMatch row ref input
>   where
>     allMatch p (a:as) (b:bs) = p a b && allMatch p as bs
>     allMatch _   []     _    = True  -- out of pattern, success
>     allMatch _   _      _    = False -- out of input, failure
> 
>     row = allMatch pixel
>
>     pixel False = const True -- blank pattern, always match
>     pixel True  = id         -- hash pattern, match on hash

This only matches if the pattern is found at $(0,0)$.  Here's a helper
to attempt all shifts of the image .

> lowerRights :: [[a]] -> [[[a]]]
> lowerRights = concatMap tails . transpose . map tails

And I can now complete the monster counter.

> countMonsters :: Tile -> Int
> countMonsters = length . filter (match seaMonster). lowerRights . tileImage

A final helper before attempting part 2:

> countHashes :: Image -> Int
> countHashes = length . filter id . concat

   > ``` { .repl }
   > λ> let image = merge assembly
   > λ> maximum $ map (countMonsters . reorient image) allSetups
   > 2
   > λ> countHashes (tileImage image) -2*countHashes seaMonster
   > 273
   > ```

Am I playing my luck a bit here?  This computation works for the
sample, but theoretically fails if a hash can be a part of multiple
sea monsters.

Does it happen in my puzzle input?  There's only one way to find out…[^no]

[^no]: Of course there isn't.  I could go the rigorous route and mark
the relevant pixels, perform their set union then difference with the
rest of the hashes.  But that would most likely take me more than a
single minute.  When the site grants us an attempt per minute, I'd be
stupid not to try my luck before implementing the complex stuff.

In the meantime, here's the rest of the code for completeness.

> main :: IO ()
> main = do
>   tiles <- parseInput <$> readFile "day20.in"
>   let assembly = reassemble tiles
>   putStrLn $ "Assembly product: " ++ show (product (cornerIds assembly))
>   let image = merge assembly
>       monsters = maximum $ map (countMonsters . reorient image) allSetups
>       roughness = countHashes (tileImage image)
>         - monsters * countHashes seaMonster
>   putStrLn $ "Water roughness: " ++ show roughness

Well, that turned out to be enough for the second gold star of the
day.  I'm not sure I can disprove sea monsters overlapping each other
by reading the statement again, so I figure it's just Eric being nice
to us.

Or maybe only to me?  O:-)

This concludes this day's solution.  Hope you enjoyed it; see you soon!
