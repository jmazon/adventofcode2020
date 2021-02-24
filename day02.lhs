---
title:       "AoC day 2: Password Philosophy"
author:      "Jean-Baptiste Mazon"
date:        "2020-12-02T10:07:55+01:00"
updated:     "2021-01-01T22:56:55+01:00"
tags:        ["advent of code", aoc2020, haskell]
description: "Validating passwords with Prelude functions"
image:       aoc-2020-02.jpeg
---

[Day 2][aoc] is the first of the year for one of the typical “one
input, two interpretations” AoC puzzles.  This post is [literate
Haskell][lit].

[aoc]: https://adventofcode.com/2020/day/2
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day02.lhs


> {-# LANGUAGE RecordWildCards #-}
> import Data.List
> import Text.Parsec
> type Parser = Parsec String ()

Each record provides two numbers, a character and a password.  The
interpretation for them varies for both parts, so I'll store them
under generic names.

> data Entry = Entry
>   { a :: !Int
>   , b :: !Int
>   , c :: !Char
>   , password :: String
>   }

I'll parse them with a straightfoward `Parsec` parser.

> entry :: Parser Entry
> entry = Entry
>   <$> number
>   <*  char '-'
>   <*> number
>   <*  char ' '
>   <*> anyChar
>   <*  string ": "
>   <*> many1 anyChar
> 
> number :: Parser Int
> number = read <$> many1 digit

And a little wrapper.

> parseInput :: String -> [Entry]
> parseInput =
>   either (error . show) id .
>   mapM (parse (entry <* eof) "input") .
>   lines

The notion of password validity for part 1 is that the character must
occur a number of times within the interval.

> valid1 :: Entry -> Bool
> valid1 Entry{..} = a <= numC && numC <= b
>   where numC = length (elemIndices c password)

For part 2, the logic is a bit more involved: exactly one position
indexed by the numbers must be equal to the character.  I'll implement
that as reading both, checking for equality to the provided character,
and validate by canonicalizing with a sort.  Being careful to adjust
the indexing's origin.

> valid2 :: Entry -> Bool
> valid2 Entry{..} =
>   sort (map ((== c) . (password !!) . pred) [a,b]) == [False,True]

And here's the `main` wrapper.

> main :: IO ()
> main = do
>   entries <- parseInput <$> readFile "day02.in"
>   print $ length $ filter valid1 entries
>   print $ length $ filter valid2 entries

This concludes today's solution.  See you soon!
