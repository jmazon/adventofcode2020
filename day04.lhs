---
title:       "AoC day 4: Passport Processing"
author:      "Jean-Baptiste Mazon"
date:        "2020-12-04T09:45:48+01:00"
updated:     "2021-01-01T18:28:48+01:00"
tags:        ["advent of code", aoc2020, haskell]
description: "Parsing the INPUT with FUNCTIONS!!!"
image:       aoc-2020-04.jpeg
---

A straightforward implementation will do the trick for [today's
puzzle][aoc].  This post is [literate Haskell][lit], starting with a
few imports and extensions.

[aoc]: https://adventofcode.com/2020/day/04
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day04.lhs

> {-# LANGUAGE LambdaCase   #-}
> {-# LANGUAGE ViewPatterns #-}
> import Data.Bifunctor
> import Data.Char
> import Data.List.Split

A passport is a set of key/value pairs.  I'll store it in an
[association list][alist].

[alist]: https://en.wikipedia.org/wiki/Association_list

> type Passport = [Field]
> type Field    = (FieldId,Value)
> type FieldId  = String
> type Value    = String

To parse, I'll split paragraphs, then split by words and break on
colon.

> parsePassport :: String -> Passport
> parsePassport = map parseField . words
> 
> parseField :: String -> Field
> parseField = second tail . break (== ':') 

In part 1, “valid” means “containing all the required fields”.

> validate1 :: Passport -> Bool
> validate1 passport = all (`elem` map fst passport) fieldIds

In part 2, “valid” means “present *and* abiding by some field-specific
rules”.  I'll define a type for a function that validates
 a field
value.

> type Validator = Value -> Bool

Three fields are validated by a numerical interval check.

> digits :: Int -> Int -> Int -> Validator
> digits len low high ds =
>   length ds == len
>   && read ds >= low
>   && read ds <= high

My input is very nice to me: it's well-formed enough that this `read`
call never fails.

Now I can define an alist of validators.

> validators :: [(FieldId,Validator)]
> validators =
>   [ ("byr",digits 4 1920 2002)
>   , ("iyr",digits 4 2010 2020)
>   , ("eyr",digits 4 2020 2030)

The “eye color” field is a simple membership check from a hardcoded
set; I can define it inline.

>   , ("ecl",(`elem` ["amb","blu","brn","gry","grn","hzl","oth"]))

The “passport id” field has two conditions, but a lambda expression
fits the bill.[^reader]

[^reader]: Yes, I know how to do without by lifting `(&&)` in the
reader applicative.  This is educational code.

>   , ("pid",\ds -> length ds == 9 && all isDigit ds)

The last two fields are a bit too complex to fit in a line.  I'll
defer to toplevel functions.

>   , ("hgt",height)
>   , ("hcl",hairColor)
>   ]

Height is a “digits” field with a unit, with differing allowed ranges
depending on the unit.

> height :: Validator
> height (break isLetter -> (ds,unit)) =
>   case unit of
>     "cm" -> digits 3 150 193 ds
>     "in" -> digits 2  59  76 ds
>     _    -> False

Hair color is a 6-digit HTML color code.  I'll check for syntax and
length by pattern-matching.

> hairColor :: Validator
> hairColor = \case
>   '#':xs@[_,_,_,_,_,_] -> all isHexDigit xs
>   _ -> False

Now I can wrap it all to check an entire passport.

> validate2 :: Passport -> Bool
> validate2 passport =
>   all (\(f,v) -> maybe False v (lookup f passport)) validators

Since the field list is the same, I can avoid repeating myself and
derive it from the validators' alist.

> fieldIds :: [FieldId]
> fieldIds = fst <$> validators

Obviously, the country id field is a red herring: since it's valid to
have it as well as not have it, and its contents are ignored, it
doesn't ever need to be checked or even appear in my code.

> main :: IO ()
> main = do
>   ps <- map parsePassport . splitOn "\n\n" <$> readFile "day04.in"
>   print $ length $ filter validate1 ps
>   print $ length $ filter validate2 ps

This concludes today's solution.  See you soon!

---

<small>
If you follow me through Twitter, you may have noticed I had a little
trouble feeling excited about this one.  If you want a more
interesting way to do it, I recommend reading up [Justin Le's
refinement types approach][jle].
</small>

[jle]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day04.md
