---
title:       "AoC day 8: Handheld Halting"
author:      "Jean-Baptiste Mazon"
date:        "2020-12-08T09:53:28+01:00"
updated:     "2020-12-29T19:22:31+01:00"
tags:        ["advent of code", aoc2020, haskell]
description: "The two variants of this year's interpreter"
image:       aoc-2020-08.jpeg
---

The title hints at the halting problem, but careful simulation is
sufficient to solve [today's problem][aoc].[^proof]  This post is a
legitimate [literate Haskell][lit] program starting with page after
page of language extensions and imports.

[aoc]: https://adventofcode.com/2020/day/8
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day08.lhs
[^proof]: But proving it on such a simplistic architecture *is* feasible.

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE NamedFieldPuns #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ViewPatterns #-}
> import           Control.Applicative
> import           Control.Monad.Trans.Accum
> import           Data.Functor.Identity
> import           Data.List
> import qualified Data.Set as S
> import           Data.Monoid

I'll take the easy route to parsing, and merely split words and
convert the argument to an integer.

> data Instruction = Instruction { operation :: String, δ :: Int }
>
> decode :: String -> Instruction
> decode (words -> [operation,argument]) =
>   Instruction{operation,δ = readSigned argument}
>
> readSigned :: String -> Int
> readSigned ('+':n) = read n
> readSigned n = read n

I shortened the argument to δ as it's only ever added to either the
instruction pointer or the accumulator.

Speaking of which, I'll `newtype` them so I don't mix them up by
mistake.

> newtype Address = Address Int deriving (Eq,Ord,Num,Real,Enum,Integral)
> newtype Value   = Value   Int deriving (Num,Show)

Now to simulate.  I'll use an open-out design, where various
components are delegated to callbacks that I'll vary between parts 1
and 2.

> simulate :: Monad m
>   => (Value -> m Value)
>   -> (Value -> m Value)
>   -> (Instruction -> m (Address,Value))
>   -> [Instruction]
>   -> m Value
> simulate loop eof eval is = go S.empty (Address 0) (Value 0) where
>   go cl ip acc
>     | ip `S.member` cl       = loop acc
>     | ip == genericLength is = eof  acc
>     | otherwise = do
>         (δ_ip,δ_acc) <- eval (is `genericIndex` ip)
>         go (S.insert ip cl)
>           (ip + δ_ip)
>           (acc + δ_acc)

The `generic`-* method names may be a bit verbose, but such is the
price of protection from integer mixups.

The components I extracted are:

* what to return when a loop is detected
* what to return when the end of the program is reached
* how to actually interpret the instructions

The first two callbacks take the value of the accumulator as an input.

The last one takes an instruction and returns the values to add to the
CPU's state, namely the instruction pointer and the accumulator.  In a
monadic context, about which I'll expand later on.

Obviously, I'll need an actual implementation.  Transcribing the
statement:

> evaluate :: Applicative m => Instruction -> m (Address,Value)
> evaluate Instruction{..} = pure $ case operation of
>   "nop" -> (Address 1,Value 0)
>   "jmp" -> (Address δ,Value 0)
>   "acc" -> (Address 1,Value δ)

The simple evaluation model is really monad-agnostic, so all it needs
is `pure` from `Applicative` to return a result.

Now I can wire things up to solve part 1.

> part1 :: [Instruction] -> Value
> part1 = runIdentity . simulate pure (error "Reached end") evaluate

Part 2 asks to make the single nop/jmp switch that makes the program
terminate.  As termination isn't an immediately observable behavior,
simulation needs to continue after a choice is made, so I'll need
backtracking.  And I'll need some form of memory to allow myself a
single change only.

So I'll use an `AccumT` monad transformer over a list monad.
Evaluation always delegates to the simple model, and then also
attempts an instruction flip if memory allows.

> evaluate' :: Instruction -> AccumT Any [] (Address,Value)
> evaluate' i@Instruction{..} = evaluate i <|> do
>   Any flipped <- look
>   add (Any True)
>   case (flipped,operation) of
>     (True,_)      -> empty
>     (False,"acc") -> empty
>     (False,"nop") -> pure (Address δ,Value 0)
>     (False,"jmp") -> pure (Address 1,Value 0)

It may be noteworthy the instruction alteration is never actually
recorded: since loops are going to result in immediate failure, just
temporarily operating differently is enough.

The wrapper sets up the monad transformer while also taking care of
rejecting simulations that lead to loops (the `const empty`
component).

> part2 :: [Instruction] -> [Value]
> part2 = flip evalAccumT (Any False) . simulate (const empty) pure evaluate'

It so happens my input has a single solution.  Here's the rest of the
code for completeness.

> main :: IO ()
> main = do
>   code <- map decode . lines <$> readFile "day08.in"
>   print $ part1 code
>   print $ part2 code

This concludes today's solution.  See you soon!
