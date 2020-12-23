---
title:       "AoC day 23: Crab Cups"
author:      "Jean-Baptiste Mazon"
date:        2020-12-23T08:30:12+01:00
tags:        ["advent of code", "aoc2020", "haskell"]
description: "Mutable linked lists in the ST monad"
image:       aoc-2020-23.jpeg
---

[Today's challenge][aoc] revolves around optimizing a circular list
stitch-and-patch procedure.  Part 1 serves as appetizer; the real meat
is in part 2, where the circle is a million items long instead of
nine, and the number of operations ten millions instead of a hundred.
This post is a [literate Haskell][lit] program.

[aoc]: https://adventofcode.com/2020/day/23
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day23.lhs

> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE RankNTypes #-}
> 
> import Control.Monad           (replicateM_,zipWithM_)
> import Control.Monad.ST.Lazy   (ST,runST)
> import Data.Array.ST           (STArray,newArray_,readArray,writeArray,getBounds)
> import Data.Char               (isDigit,digitToInt)

The gist of the procedure isn't too hard to implement.  It could
reasonably be done for part 1 using a `Seq`uence of Ints, or even a
simple list.

The issue is scaling it up to part 2.

The key observation is that no matter the representation, we need a
fast access operation to a move's destination.  This is trickier than
it looks.

But first the cause.  The procedure involves moving three adjacent
cups from a spot on the circle to another.  This is a globally
logarithmic operation on `Seq`.  So our general complexity is $$O
\left( R × (\text{cost}(\text{access cursor}) +
\text{cost}(\text{access destination}) + \log N ) \right)$$

The cost of accessing the cursor can easily be kept $O(\log N)$ by
storing its position alongside the circle, or by always shifting the
`Seq` to it and keeping it at index 0.  The cost of accessing the
destination, on the other hand, is that of a linear scan if we don't
do anything smart about it.  That would bring the complexity to
$O(R×N)$, which is too much, so we *have* to do something about it.

Any attempt to store indices is going to have to confront itself with
the cost of updating them when the strings of 3 are moved around.
It's going to be too much as well.

So an idea would be to store a cup's destination in a representation
that isn't affected by our shifting stuff around.  One way to do this
would be with an explicit node representation:

``` Haskell
data Node = Node { nodeLabel :: Int, nodeDestination :: Node }
```

(This is isomorphic to the standard Haskell list.)

But that doesn't solve everything.  We still need to keep track of the
editing that goes around.  And using any container-based approach hits
the same class of problem: we either need to update half of the circle
at each move or can't do anything useful with the `Node` accessor.

The solution is to move the structure information to the node *and
make it mutable*.

``` Haskell
data Node s = Node
  { nodeLabel :: Int
  , nodeNextClockwise :: STRef s Node
  , nodeDestination :: Node
  }
```

Now the whole circle can be addressed from just a `Node`.  We still
need to allocate all of them, and we know how many we have in advance,
so we can store them in an array.

``` Haskell
type State s = Array Int (Node s)
```

But… if they have a stable offset in the array, like, say, their
label, we don't need to store a destination link at all, we can
compute it in constant time given a label!

``` Haskell
data Node s = Node
  { nodeLabel :: Int
  , nodeNextClockwise :: STRef s Node
  }
```

But then we can just simplify that node type to a simple integer: its
clockwise neighbor .

> type State s = STArray s Int Int

So my state is an array of cups indexed by cups, representing the next
one in the circle.  With the cup labels being 1-based, this has the
added bonus that I can store the link to the current cup right there,
at index 0.

Now the data type is stable, I can implement the move as a
straightforward transcription of the statement to this linked list
encoding.

I'll start by retrieving the current cup's label/index.

> move :: State s -> ST s ()
> move env = do
>   current <- readArray env 0

Then the cups are picked up.

>   pick@[pickFirst,_,pickLast] <- toListN current 3 env

For list editing purposes, I only need to know the first and last.
Next I cut them out of the circle by having the current cup skip to
the first cup after the pickup.

>   next <-readArray env pickLast
>   writeArray env 0 next

The destination is usually one less than the current by default.  But
it does happen that we need to skip a few numbers.

>   (_,n) <- getBounds env
>   let (dest:_) = filter (`notElem` pick) ([current-1,current-2..1] ++ [n,n-1..])

I can now insert the segment right next to the destination cup.

>   suffix <- readArray env dest
>   writeArray env dest pickFirst
>   writeArray env pickLast suffix

There's no need to rewire the picked up cups internally: they remain
in the same order with regard to each other.

Finally I update the link to the current cup.

>   writeArray env current next

I used a `toList` derivative so I didn't have to chain too many
`readArray` operations, but it doesn't really follow the
`Foldable` typeclass signatures.  I define it as such:

> toListN :: Int -> Int -> State s -> ST s [Int]
> toListN start count env  = go start count where
>   go _ 0 = pure []
>   go i c = readArray env i >>= \n -> (n : ) <$> go n (c-1)

Initially I used `unsafeInterleaveST` and a lazy infinite list, but I
figured that didn't buy much when I actually always knew in advance
how many items I needed.

Now to generate the starting structure.  We're following a permutation
list pattern, so initialization is performed in pseudo-random[^random]
order.  We know we're not leaving any holes by responsibility of the
caller to not provide a 0 or any duplicates.  We know it's well-formed
by further responsibility of the caller to include the entire $[0,n]$
range.

[^random]: As far as the array initializer knows.  It's highly regular
for values above 9.

> fromList :: [Int] -> ST s (State s)
> fromList l = do
>   let n = length l
>   a <- newArray_ (0,n)
>   zipWithM_ (writeArray a)
>     (  [0]    ++         l         )
>     ([head l] ++ tail l ++ [head l])
>   pure a

Now playing a game is a simple matter of intializing the structure and
repeatedly applying moves.  I allow information extraction by taking
an `ST`-based continuation as a parameter.

> play :: [Int] -> Int -> (forall s. State s -> ST s a) -> a
> play l n f = runST $ do
>   env <- fromList l
>   replicateM_ n (move env)
>   f env

I can now perform the part 1 game.

  > ``` { .repl }
  > λ> play sample 100 $ toListN 1 8
  > [6,7,3,8,4,5,2,9]
  > ```

For part 2, I'll use a helper to expand the initial list.

> unMistake :: [Int] -> [Int]
> unMistake xs = xs ++ [maximum xs+1 .. 1000000]

Playing the game takes significant time interpreted.  I packaged it
with a `main` function for compiled execution.  It takes about
10 seconds.

> main :: IO ()
> main = do
>   input <- map digitToInt . filter isDigit <$> readFile "day23.in"
>   print $ play input 100 $ toListN 1 8
>   print $ play (unMistake input) 10000000 $ fmap product . toListN 1 2

This concludes today's solution.  Hope you enjoyed it, and see you
tomorrow!
