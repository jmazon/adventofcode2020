---
title:       "AoC day 15: Rambunctious Recitation"
author:      Jean-Baptiste Mazon
date:        2020-12-15T10:27:40+01:00
tags:        ["advent of code", "aoc2020", "haskell"]
description: Solving day 15 by trial and error
image:       aoc-2020-15.jpeg
---

[Day 15 part 1][aoc] presents us with a classic “implementation”
problem.  We are to generate an integer sequence, with a starting
(multiple-number) seed, and a simple rule to define the next integer:
it's the distance between the two previous occurences of the last
number.

[aoc]: https://adventofcode.com/2020/day/15

As such, it doesn't ring a bell just yet[^reddit], but the definition
is clear enough.  And I only need to compute 2020 terms, so there's
not much to worry about: a straightforward implementation is a valid
way of proceeding for now.

[^reddit]: ~~But I'll be sure to check out [the subreddit][sub] once
I'm done.~~ So it's a Van Eck's sequence.  I'd already implemented
them, but it apparently hasn't left that much of an impression that
I'd remember them on sight.

[sub]: https://www.reddit.com/r/adventofcode/comments/kdf85p/2020_day_15_solutions/

AoC is in the morning for me, so the coffee hasn't really kicked in
yet.  I don't have the perfect data representation that springs to
mind by reflex.  Not to worry, I can use top-down
compile-error-message-driven design.

Let's write a simple transcription of the game logic using do-notation:

``` Haskell
game starter = do
  forM_ starter play
  forever $ do
    lastNumberAge >>= \case
      Nothing  -> play 0
      Just age -> play age
```

It complains about `LambdaCase` not being active and `forM_` not being
defined.  That's easy enough to fix.

``` Haskell
{-# LANGUAGE LambdaCase #-}
import Control.Monad
```

The remaining errors are:

<blockquote class="compiler-errors">
Variable not in scope: play :: Int -> m b0<br>
Variable not in scope: lastNumberAge :: m (Maybe t0)
</blockquote>

Let's start with the latter.  How would I extract the previous
number's age?  Let's start simple.

``` Haskell
import Control.Monad.State.Class
import qualified Data.IntMap.Strict as Map

lastNumberAge = do
  n <- gets stLastNum
  Map.lookup n <$> gets stAge
```

Ok, that was a cop out.  It seems reasonable enough to maintain some
state that remembers the last number played as `stLastNum`.  It seems
a bit less reasonable to remember every number's age as the game goes,
since I'd have to update it all the time.  Better keep track of time
instead, and infer the age.

``` Haskell
lastNumberAge = do
  n <- gets stLastNum
  cur <- gets stTurn
  fmap (cur -) . Map.lookup n <$> gets stTurnPlayed
```

<blockquote class="compiler-errors">
Variable not in scope: play :: Int -> m b0<br>
Variable not in scope: stLastNum :: s0 -> Map.Key<br>
Variable not in scope: stTurn :: s0 -> b<br>
Variable not in scope: stTurnPlayed :: s0 -> Map.IntMap b<br>
</blockquote>

We're getting a better understanding of the state's shape.

Let's continue with `play`.  A reasonable way to “say” a number would
be to simply output it to a `Writer` monad instance.  The interesting
part is how to maintain the state as it goes.

``` Haskell
{-# LANGUAGE FlexibleContexts,NamedFieldPuns #-}
import Control.Monad.Writer.Class

play n = do
  St{stTurn,stTurnPlayed} <- get
  put St { stTurn = stTurn + 1
         , stLastNum = n
         , stTurnPlayed = Map.insert n stTurn stTurnPlayed
         }
  tell [n]
```

That looks like it could work!  What I'm missing now is actually
defining the `St` structure with the fields listed in the error
message.

``` Haskell
data St = St { stTurn       :: !Int
             , stLastNum    :: Int
             , stTurnPlayed :: !(Map.IntMap Int)
             }
st0 :: St
st0 = St { stTurn       = 0
         , stLastNum    = error "No last number at game start!"
         , stTurnPlayed = Map.empty
         }
```

All of the “variable not in scope” errors are gone.  All that remains
are complaints about the type being ambiguous.  This can be addressed
by defining a wrapper to run the game as a pure function.

``` Haskell
import Control.Monad.RWS.Lazy

runGame :: [Int] -> [Int]
runGame starter = snd $ evalRWS (game starter) () st0
```

Let's try it on the example!

<blockquote class="repl">
λ> runGame [0,3,6] !! 2019<br>
1
</blockquote>

Something's wrong.  The expected answer is 436![^fact] What did I
miss?  Let's trace the actual first few values.

[^fact]: I'm sorry.  [The expected answer is actually 436.](https://twitter.com/kj_cheetham/status/1149966666836656128)

<blockquote class="repl">
λ> take 10 $ runGame [0,3,6]<br>
[0,3,6,1,1,1,1,1,1,1]
</blockquote>

The first three numbers are correct.  Whew!  But then it seems like
I'm not only failing to output the correct next number, namely 0,
but additionally the logic is getting stuck in a loop.

The reason for this is actually quite simple if you trace the
algorithm.  When the third and final starter number, namely 6, is
played, it is added to the history map as having been played at
turn 3.  But when I request its age using the `lastNumberAge`
function, it will compute the age difference between the turn being
computed, namely 4, and the turn I just stored in the map, resulting
in 1.

The next number computed makes the exact same mistake, hence the loop.
Oops!

So the root cause is that with the current algorithm, I never have the
last number played's previous two dates available at the same time.
So I can't compute a valid difference.  To correct this, I'll shift
the computation of the last number's age to when I still know its
previous date.

So I'll replace the `stLastNum` state field with `stLastNumAge`, and
move the computation to the `play` function.

``` Haskell
st0 :: St
st0 = St { stTurn       = 0
         , stLastNumAge = error "No last number at game start!"
         , stTurnPlayed = Map.empty
         }

play n = do
  St{stTurn,stTurnPlayed} <- get
  put St { stTurn       = stTurn + 1
         , stLastNumAge = (stTurn -) <$> Map.findWithDefault stTurn n stTurnPlayed
         , stTurnPlayed = Map.insert n stTurn stTurnPlayed
         }
  tell [n]

lastNumberAge = gets stLastNumAge
```

<blockquote class="repl">
λ> take 10 $ runGame [0,3,6]<br>
[0,3,6,0,3,3,1,0,4,0]
</blockquote>

Much better.  I ran it with my puzzle input, got my gold star and
unlocked part 2.

Part 2 was about doing the same thing 30 million times.  Well, it's
not that big a number, especially for offline processing.  My code is
already $O\left(N \log N\right)$,[^false] there's not too much point
optimizing it before I at least get an estimate of how long it will
take.

[^false]: This is probably false.  But not so blatantly.  I'm using
the lazy writer monad on `[Int]`, which likely incurs some
concatenation penalty.  But then again not that much considering the
use site.  I've run it using a `DList` instead and it took the same
time.

As it turned out, it needed less than 45 seconds to compute when
compiled.  Less than two minutes interpreted.[^expect] So really not
worth the engineering time to reduce to linear or better.  Maybe some
other time.

[^expect]: I expected more difference.  I'll have to measure it more
reliably.

---

This concludes day 15's solution.  Here's the full code, with the
additional simplifications of:

* directly considering a new number to have an age of 0 and skirting
  the `Maybe` wrapper.[^reasonable]
* fusing `lastNumberAge` and `play` by having the last number age be
  the monadic return value of `play`.  This lets me drop
  `stLastNumAge` from the state.

[^reasonable]: And I don't feel type-dirty for doing it.

> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE NamedFieldPuns #-}
> import           Control.Monad.RWS.Lazy
> import qualified Data.IntMap.Strict as Map
> import           Data.Void
>
> game :: [Int] -> RWS () [Int] St Void
> game starter = do
>   forM_ starter play
>    -- That next 0 is only valid if the last
>    -- starter number is unique.  Mine is.
>   fix (play >=>) 0
>
> data St = St { stTurn       :: !Int
>              , stTurnPlayed :: !(Map.IntMap Int)
>              }
> st0 :: St
> st0 = St { stTurn = 0, stTurnPlayed = Map.empty }
>
> runGame :: [Int] -> [Int]
> runGame starter = snd $ evalRWS (game starter) () st0
>
> play :: Int -> RWS () [Int] St Int
> play n = do
>   St{stTurn,stTurnPlayed} <- get
>   put St { stTurn       = stTurn + 1
>          , stTurnPlayed = Map.insert n stTurn stTurnPlayed
>          }
>   tell [n]
>   pure $ stTurn - Map.findWithDefault stTurn n stTurnPlayed
>
> main :: IO ()
> main = do
>   starter <- read . (\s -> "["++s++"]") <$> getContents
>   print $ runGame starter !! 2019
>   print $ runGame starter !! 29999999
