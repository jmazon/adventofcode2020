---
title:       "AoC day 18: Operation Order"
author:      "Jean-Baptiste Mazon"
date:        2020-12-18T10:14:16+01:00
tags:        ["advent of code", "aoc2020", "haskell"]
description: "Ignoring centuries of convention with Parsec"
image:       aoc-2020-18.jpeg
---

The [puzzle of the day][aoc] prompts us to perform antinatural[^used]
math.  This looks like the kind of puzzle that would be easier solved
with two regex replacements and an `eval`, but I started this year in
Haskell, let's keep at it.  As usual in this series, this post is [a
literate Haskell program][lit].

[aoc]: https://adventofcode.com/2020/day/18
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day18.lhs

[^used]: Well, it's only antinatural if you spent years letting the
standard way seep in.

I'll use parser combinator library [`Parsec`][parsec].

[parsec]: https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html

> import Text.Parsec
> type Parser = Parsec String ()

I don't want to get lost in tokenization or risk forgetting to skip
spaces, so I'll just simplify the input before processing it.

> extractMath :: String -> [String]
> extractMath = lines . filter (/= ' ')

Now to write the parser.  It's rather straightforward.  An expression
is a flat chain of operations performed on subexpressions.  Those
subexpressions are either a literal number or another expression.

`Parsec` has[^other] a useful combinator that will directly perform
the operation in a flat, left-associative manner.  So we can use that
directly.

[^other]: `Parsec` has it, but I think just about every other parser
combinator library has it too.

> expr, num :: Parser Int
> expr = (paren expr <|> num) `chainl1` op <?> "expression"
> num = read <$> many1 digit <?> "number"
> 
> op, plus, times :: Parser (Int -> Int -> Int)
> op = plus <|> times <?> "operation"
> plus  = (+) <$ char '+'
> times = (*) <$ char '*'

This makes use of a rather generic helper I extracted for readability.

> paren :: Parser a -> Parser a
> paren rec = between (char '(') (char ')') rec <?> "parenthesized group"

And… that's all there is to it!  Parser combinators don't have innate
preference for the human way of ordering operations, and they'll
provide us with the results we want unconfused:

  > ``` { .repl }
  > λ> traverse (parse (expr <* eof) "expression") $ extractMath sample
  > Right [51,26,437,12240,13632]
  > ```

Now for part 2.  The operations *are* ordered here.  Let's adjust the
parser for that.

> exprV2 :: Parser Int
> exprV2 = (paren exprV2 <|> num) `chainl1` plus `chainl1` times <?> "advanced math"

  > ``` { .repl }
  > λ> traverse (parse (exprV2 <* eof) "expression") $ extractMath sample
  > Right [51,46,1445,669060,23340]
  > ```

The parser was unimpressed by the plot twist.  Here's the end of the
code for completeness.

> main :: IO ()
> main = do
>   math <- extractMath (/= ' ') <$> readFile "day18.in"
>   print $ sum <$> traverse (parse (expr   <* eof) "basic math")    math
>   print $ sum <$> traverse (parse (exprV2 <* eof) "advanced math") math

This concludes day 18.  I hope you enjoyed it.  See you tomorrow!
