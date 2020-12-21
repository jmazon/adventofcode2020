---
title:       "AoC day 21: Allergen Assessment"
author:      "Jean-Baptiste Mazon"
date:        2020-12-21T11:49:24+01:00
tags:        ["advent of code", "aoc2020", "haskell"]
description: "Constraint programming using set operations"
image:       aoc-2020-21.jpeg
---

I almost had a panic attack when I got to [today's challenge][aoc].
It's early in the morning[^always], half of the Internet is laughing
at how easy today's puzzle is in comparison to yesterday's…  and when
I read the statement, all I can see is an intractable high-variate
constraint programming task, whereas yesterday was quite approachable.

[aoc]: https://adventofcode.com/2020/day/21
[^always]: By definition of “morning”.

This post is a [literate Haskell][lit] program.  Please skip over the
imports while I go grab myself a cup of coffee.

[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day21.lhs

> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE TupleSections #-}
> import           Control.Applicative (liftA2)
> import           Data.Foldable
> import           Data.List           (permutations)
> import           Data.Map.Strict     (Map,(!))
> import           Data.Set            (Set,unions,intersection,(\\),isSubsetOf)
> import           Text.Parsec
> import qualified Data.Map.Strict as Map
> import qualified Data.Set as Set
> type Parser = Parsec String ()

I'll use a simple line-based `Parsec` parser.  Not that that much
power is really needed for the input format, but I've been writing
them all month, so it's currently my best choice in terms of
whipuptitude.

> parseRecipes =
>     either (error . show) id          .
>     traverse (parse parseFood "food") .
>     lines
>   where
>     parseFood = liftA2 Recipe ingredients allergens <* eof
>     ingredients = Set.fromList <$> many (Ingredient <$> word)
>     allergens = Set.fromList <$> between (string "(contains ") (char ')')
>                                    ((Allergen <$> word) `sepBy1` string ", ")
>     word = many1 letter <* optional spaces

You've been reading my parsers all month long as well, so there's
really not much to comment on anymore.  For a summary, the input is a
list of recipes.

> parseRecipes :: String -> [Recipe]

A recipe formalizes a relation between a list of ingredients and a
list of allergens.  There's no expected repetition, I'm storing as
sets for later use.

> data Recipe = Recipe
>   { rIngredients :: Set Ingredient
>   , rAllergens   :: Set Allergen
>   }

Ingredients and allergens are provided as strings.  One set happens to
be ~~readable~~ in English and not the other, but that's storytelling.
From a solving point of view, it doesn't matter.  What matters is
they're a different type of string, so I'll `newtype` them to avoid
using one kind instead of the other when I'm not paying attention.

> newtype Allergen   = Allergen   String deriving (Eq,Ord)
> newtype Ingredient = Ingredient String deriving (Eq,Ord)

Poking around the data, my input appears to consist in 39 recipes,
listing a total of 8 allergens spread among a list of 200 ingredients.

Now to actually identify guaranteed-safe ingredients as the statement
requests.

This is where I initially got stuck.  I couldn't for the life of me
figure how an ingredient could ever be ruled out from being
allergenic.  After all, allergens are never guaranteed to be listed,
so what's preventing an ingredient from containing trace amounts of
that spooky allergen, and not being reported in a single food item as
the statement explicitly allows?  I have no way of telling it apart
from an actually safe ingredient, have I?  So there can be no safe
ingredients at all, this statement doesn't make any sense!

As the coffee started to kick in, I finally found the key sentence.
Hidden in plain sight as the leading one in paragraph 4.

  > Each allergen is found in exactly one ingredient.

Oooooooh.  Well *that*'s better.

Each recipe gives us a bit of information about the allergens.  It
doesn't really tell us which is which unless we're given pure
ingredients as foods[^happen], but it does constrain the set of
ingredients that are known to carry those allergens.  By
cross-referencing them with the constraints from other recipes, I can
get at least *some* information as to which ingredients could carry
each allergen.

[^happen]: Which doesn't happen in my puzzle input, unfortunately.

I'll work bottom-up, first extracting a map of possible carriers from
a recipe.

> allergenCandidates :: Recipe -> Map Allergen (Set Ingredient)
> allergenCandidates Recipe{..} =
>   Map.fromList $ map (,rIngredients) (toList rAllergens)

And then I'll work across recipes to generate the set of ingredients
who remain in any allergen's carrier set.

> suspiciousIngredients :: [Recipe] -> Set Ingredient
> suspiciousIngredients =
>   fold . Map.unionsWith intersection . map allergenCandidates

  > ``` { .repl }
  > λ> suspiciousIngredients recipes
  > fromList [csqc,fnntr,gzvsg,jlsqx,lvv,pmz,tr,xblchx]
  > ```

Wow.

Only 8 suspicious ingredients.  For a total of 8 known allergens!
That cross-referencing didn't bring up “a bit more” information, it
gave me “just about all” of it.

And definitely enough to complete part 1.  I'll just write a small
helper to count how many ingredients from a set occur in a recipe.

> occurrence :: Set Ingredient -> Recipe -> Int
> occurrence is = Set.size . intersection is . rIngredients

And I've got all I need to complete.

  > ``` { .repl }
  > λ> let ingredients = unions (rIngredients <$> recipes)
  > λ> let dangerousIngredients = suspiciousIngredients recipes
  > λ> let safeIngredients = ingredients \\ dangerousIngredients
  > λ> sum $ occurrence safeIngredients <$> recipes
  > 2176
  > ```

For part 2, classification isn't enough anymore, we need to pair
allergens and ingredients individually.

I could implement a backtracking search as I did a few times already
this month.  But the problem space here is really small.  An
exhaustive search is going to be much easier to write.

We're looking for a one-to-one mapping from allergen to ingredient.
More specifically, we're asked to provide it in alphabetical order of
allergens.  I'll switch to lists so I can introduce that notion of
ordering.

> type Matching = [(Allergen,Ingredient)]

Building a matching from two ordered lists of allergens and
ingredients is trivial.

> match :: [Allergen] -> [Ingredient] -> Matching
> match = zip

The specific canonical matching we're looking for is in order of
allergens, so I'll refine.

> canonMatching :: Set Allergen -> [Ingredient] -> Matching
> canonMatching allergens = match (Set.toAscList allergens)

A matching is the correct one if it can't be proved wrong.  To take a
jab at it, I'll check its consistency with regard to a provided
recipe.

> consistent :: Matching -> Recipe -> Bool
> consistent matching = let m = Map.fromList matching in \Recipe{..} ->
>   let dangerousIngredients = Set.fromList (map (m !) (toList rAllergens))
>   in dangerousIngredients `isSubsetOf` rIngredients

If we know the full allergen-to-ingredient relation list, we know a
food item's actual allergen list exactly.  So the consistency check is
simply that the reported list of allergens is indeed a subset of the
full list of contained allergens.

By checking for consistency across all recipes, I can filter the
choices as good as can be.

> validMatching :: Matching -> [Recipe] -> Bool
> validMatching matching = all (consistent matching)

  > ``` { .repl }
  > λ> let validOrdering o = validMatching (canonMatching allergens o) recipes
  > λ> filter validOrdering (permutations (toList dangerousIngredients))
  > [[lvv,xblchx,tr,gzvsg,jlsqx,fnntr,pmz,csqc]]
  > ```

And that's all there is to it!  Here's the end of the program for completeness.

> main :: IO ()
> main = do
>   recipes <- parseRecipes <$> readFile "day21.in"
>   let ingredients = unions (rIngredients <$> recipes)
>       allergens   = unions (rAllergens   <$> recipes)
>   let dangerousIngredients = suspiciousIngredients recipes
>       safeIngredients = ingredients \\ dangerousIngredients
>   print $ sum $ occurrence safeIngredients <$> recipes
>   let validOrdering o = validMatching (canonMatching allergens o) recipes
>   let [canonical] = filter validOrdering
>                       (permutations (toList dangerousIngredients))
>   print canonical
>
> -- small hack to avoid too much editing when copy-pasting the answer
> instance Show Ingredient where show (Ingredient i) = i

This concludes this day's solution.  Coffee aside, it was actually
harder to present than to code.  :-)

See you tomorrow!
