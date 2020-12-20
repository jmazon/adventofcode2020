---
title:       "AoC day 19: Monster Messages"
author:      "Jean-Baptiste Mazon"
date:        2020-12-19T12:48:34+01:00
tags:        ["advent of code", "aoc2020", "haskell"]
description: "I put a parser in your parser so you can verify push-down regular languages."
image:       aoc-2020-19.jpeg
---

[Today's problem][aoc] is a simple language verification.  Well,
“simple” as in “simply defined”.  This post is [literate
Haskell][lit], so let's get the program header out of the way so we
can talk business instead.

[aoc]: https://adventofcode.com/2020/day/19
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day19.lhs

> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE ViewPatterns #-}
> import           Prelude hiding ((+),(*))
> import qualified Prelude
> import           Control.Applicative (liftA2)
> import           Data.Function       (fix)
> import           Data.Functor        (void)
> import           Data.Set            (Set,member)
> import qualified Data.Set    as Set
> import           Data.IntMap         (IntMap,(!),insert)
> import qualified Data.IntMap as Map
> import           Data.List.Split     (linesBy)
> import           Text.Parsec
> import           Text.ParserCombinators.ReadP (ReadP,(+++),readP_to_S)
> import qualified Text.ParserCombinators.ReadP as ReadP
> import           Text.Regex.PCRE
> type Parser = Parsec String ()

We are provided with the language grammar and some messages.  Let's
run some statistics so i have a better understanding of where I'm
going.

  >     133 rules, 461 messages
  >     Longest message: 96 characters
  >     Total messages' length: 17224 characters

There are many angles of attack to the language decision problem.
Getting a feeling of its structure goes a long way towards choosing
the right path.  (I don't want to implement [CYK][cyk]!  It's too
abstract to be fun.)

[cyk]: https://en.wikipedia.org/wiki/CYK_algorithm

Let's parse and analyze the grammar to get a better feeling of it.
Starting with a few type definitions.

> type Grammar     = IntMap Rule
> type Rule        = Either NonTerminal Terminal
> type NonTerminal = [[Int]]
> type Terminal    = Char
> type Message     = String

That `NonTerminal` type needs commenting.  From the statement, and
matched by my input file, a nonterminal production is a choice between
one or more sequences of recursive productions.  I represent it as a
list of lists of `Int`egers.  The integer is a recursive rule
referenced by its index; the inner list is the sequence; the outer
list is the choice.

Let's parse.  This seems like the simplest grammar I've drawn `Parsec`
for to date.  If you've been following this series, you shouldn't need
much hand holding anymore.

> rule :: Parser (Int,Rule)
> rule = (,)
>   <$> (num <* string ": ")
>   <*> (Left <$> nonTerminal <|> Right <$> terminal)
> 
> nonTerminal :: Parser NonTerminal
> nonTerminal = many1 num `sepBy1` string "| "
> 
> num :: Parser Int
> num = read <$> many1 digit <* spaces
> 
> terminal :: Parser Terminal
> terminal = between (char '"') (char '"') anyChar

For ease of REPL, let's package it into an environment structure from
which I could easily address the fields with `NamedFieldPuns` or
`RecordWildCards`.

> data Env = Env { grammar :: Grammar, messages :: [Message] }
> 
> parseInput :: String -> Env
> parseInput (linesBy null . lines -> [rawRules,messages]) = Env{..}
>   where Right grammar = Map.fromList <$>
>           traverse (parse (rule <* eof) "rule") rawRules

I[^row] have a small number (461) of queries to perform, so I can
probably afford a complex and/or suboptimal per-message algorithm.  On
the other hand, I have 133 rules, which is large.  From the cursory
glance at the actual messages, who seem to be binary[^binary], I'm
looking at an approximate worst case of $2^131\approx2,7×10^31$
productions.  The statement is right in saying it's finite.  It's
still not tractable.

[^row]: I, and most likely the rest of the world too.
[^binary]: Binary as in: from an alphabet of cardinality 2.

Since we're given the grammar as a set of productions, we can actually
compute its total number of productions efficiently—*i.e.*
linearly—working from the terminals up.  A terminal's number of
productions is trivially 1.  A sequence's number of productions is the
product of the number of productions of its components: we're
enumerating by choosing one per.  A choice's number of productions is
the sum of the number of productions of its components: we're picking
from either, but not simultaneously.

This gives the grammar a nice [ring structure][ring].  Let's make a
typeclass out of it, see if I can exploit it later.

[ring]: https://en.wikipedia.org/wiki/Ring_(mathematics)

> class GrammarRing r where
>   fromChar :: Char -> r
>   (+) :: r -> r -> r
>   (*) :: r -> r -> r

Now let's express the language word count in terms of it.

> instance GrammarRing Int where
>   fromChar _ = 1
>   (+) = (Prelude.+)
>   (*) = (Prelude.*)

With this setup, counting the words is now only a matter of converting
the grammar's set of rules into this structure, recursively
dereferencing grammar rules by index.  I'm representing the recursion
with an explicit input parameter, so it can be externalized and acted
upon depending on context.

> toRing :: GrammarRing r => Grammar -> IntMap r -> IntMap r
> toRing grammar ringMap = fmap ruleToRing grammar
>   where
>     ruleToRing (Right c) = fromChar c
>     ruleToRing (Left refss) = foldr1 (+) (map seqToRing refss)
>     seqToRing refs = foldr1 (*) (map refToRing refs)
>     refToRing ref = ringMap ! ref

And here's a wrapper to:

* tie the knot around the recursively-defined map of production count
  by rule
* extract the one for rule 0.

> rawLanguageSize :: Grammar -> Int
> rawLanguageSize grammar = fix (toRing grammar) ! 0

  > Raw language size: 2097152

2 million words… that's not so bad.  It's small enough that I could
just generate them all to put in a set structure to verify each
message against.

How will I generate them?  By exploiting the ring structure again.

> instance GrammarRing (Set Message) where
>   fromChar c = Set.singleton [c]
>   a * b = Set.fromList $ liftA2 (++) (Set.toList a) (Set.toList b)
>   (+) = Set.union

A terminal symbol represents the word composed of that symbol only.  A
nonterminal sequence represents any word made by concatenating a word
from the first language with one from the second.  I use the list
~~monad~~ applicative's distributive property to implement that.  A
nonterminal choice represents any word from either of its components,
so its language is the union of theirs.

The wrapper helper looks very similar to the counting one.

> genLanguage :: Grammar -> Set Message
> genLanguage grammar = fix (toRing grammar) ! 0

Yay genericity!

  > Actual language size: 2097152

The language set's size is the exact same as the number of production.
It's not too important as both are tractable, but it does mean the
grammar is *unambiguous*: any word from the language it produces can
only be produced in a single way.

Now I can efficiently count the number of valid messages and glean my
gold star.[^lies]

[^lies]: I actually used the part 2 algorithm for both of my stars,
but couldn't resist doing it the less efficient way to demonstrate the
`GrammarRing` abstraction.

> test :: (Message -> Bool) -> [Message] -> Int
> test p = length . filter p
>
> performTest :: [Message] -> String -> (Message -> Bool) -> IO ()
> performTest msgs lbl p =
>   let n = test p msgs
>   in putStrLn $ lbl ++ ": " ++ show n ++ " messages"

  > ``` { .repl }
  > λ> performTest messages "Finite language" (`member` genLanguage grammar)
  > Finite language: 156 messages
  > ```

Part 2 introduces loops in the grammar.  What does that change,
concretely?

With the path I took for part 1, it changes a lot of things.  The
language's cardinality is now infinite.  It could not be efficiently
computed as I did earlier, since the map's value at index 8 would be a
strict function of itself, which isn't well-defined for any of the
evaluation models GHC provides.

I'm going to have to change my approach.

  > 8: 42 | 42 8

Looking closer at the rule 8 change, we can see it's a choice of
either production 42, or a concatenation of production 42 with
production 8.  Iterating mentally, this could be summarized as “one or
more repetitions of production 42”.

This hints at something.  What class of languages can be inductively
defined by atomic primitives, concatenation and choice?  That's the
definition of [regular languages][reg]!

[reg]: https://en.wikipedia.org/wiki/Regular_language

Regular languages can be validated by an [NFA][nfa] derived from their
grammar.  Said NFA can either be simulated, or replaced with an
equivalent [DFA][dfa], with whom validation is even more
straightforward.

[nfa]: https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton
[dfa]: https://en.wikipedia.org/wiki/Deterministic_finite_automaton

Or I could just be ~~lazy~~ efficient and express the language as a
[regular expression][regex] to let a specialized library take care of
it.  The conversion is just as mechanical as the previous two I've
detailed.

[regex]: https://en.wikipedia.org/wiki/Regular_expression

> instance GrammarRing String where
>   fromChar c = [c]
>   a + b = a ++ "|" ++ b
>   a * b = a ++ b

I'm delegating parenthesizing to the wrapper, to ease the bracket
load.  This makes use of the fact there's a maximum single level of
choice per rule.

> genRegex :: Grammar -> String
> genRegex grammar = anchor $ fix (fmap paren . toRing grammar) ! 0
> 
> paren, anchor :: String -> String
> paren s = "(" ++ s ++ ")"
> anchor s = "^" ++ s ++ "$"

Let's check this is actually equivalent to our previous way of doing.

  > 156 messages match rule 0 by regex.

Seems good.

So now all I have to do is patch up rules 8 and 11 and launch the
generator again, right?

Of course not.

Letting the generator loose on a recursive definition is the same
recipe for disaster I would have had if I'd tried to count or
enumerate the recursive language: it's not well-defined either.  The
GHC runtime can do of decent job of detecting it and aborting when run
single-threaded, but it's still not going to give a useful result.

Instead I'll patch rule 8 by hand.  The [PCRE][pcre] syntax for a
nonempty [Kleene star][star] is a plus character.

[pcre]: http://pcre.org/
[star]: https://en.wikipedia.org/wiki/Kleene_star

``` Haskell
rule8 g = g!42 ++ "+"
```

Oh.  Did I just forget about rule 11?

  > 11: 42 31 | 42 11 31

On the surface, it looks kind of similar.  Rule 11 generates either
the concatenation of productions 42 and 31, or the concatenation of
productions 42, recursive production 11, and production 31.

The bad news is, this makes it [not a regular language][pushdown]
anymore.  The good news is, PCRE handles it just fine anyway.

[pushdown]: https://en.wikipedia.org/wiki/Nested_word

The syntax to invoke a recursive pattern is normally
[<code>(?*N*)</code>][recre], where $N$ is a relative pointer to the
targeted capture buffer.  But that's going to be unwieldy here where
the regex is generated: I don't want to have to count the number of
opening parentheses that occur between pattern start and recursion.
So I'll use a [named pattern][named] and [reference][ref]
instead.[^unique]

[recre]: https://perldoc.perl.org/perlre#(?PARNO)-(?-PARNO)-(?+PARNO)-(?R)-(?0)
[named]: https://perldoc.perl.org/perlre#(?%3CNAME%3Epattern)
[ref]: https://perldoc.perl.org/perlre#(?&NAME)

[^unique]: I haven't delved too much into what ought to happen if
production 11 occurred multiple times in the rule 0 productions.  It
doesn't in *my* input.  I can see four reasonable behaviors: the first
named definition wins; the closest previous named definition wins; the
last named definition wins; no one wins and an error is raised.  By
construction, I'd always generate the same regex fragment for rule 11,
so any of the first three would be indistinguishable to me.  Case 4
would require me to think some more, but at least I wouldn't blindly
get a wrong answer.

> genRegex2 :: Grammar -> String
> genRegex2 grammar = anchor $ fix (patch . fmap paren . toRing grammar) ! 0
>   where
>     patch = (insert 8 =<< rule8) . (insert 11 =<< rule11)
>     rule8 g = g!42 ++ "+"
>     rule11 g = concat [ "(?<ELEVEN>", g!42, "(?&ELEVEN)?", g!31, ")"]

And this solves part 2.

---

**Bonus**: I used `Parsec` to parse the grammar, can't I use it to
parse the messages as well?

Well… not really.  `Parsec` is a predictive parser, its model of
choice is to attempt the next branch only if the first failed without
consuming any input.  Using `try`, it can be made to attempt the next
branch if the first one failed even if it consumed input.

But take a look at patched rule 8 above.  With a direct mapping to
`Parsec`, its second branch can never succeed: either the first one
does and the second is never attempted; or the first one fails and the
second is attempted.  But since the first is a prefix of the second,
the second is bound to fail as well.

I'm not aware of a way to make `Parsec` backtrack over the choice
point after a successful first alternative match.  I could reorder
rule 8, but there's no saying that sort of situation doesn't happen
elsewhere, or indirectly.  Maybe I could rewrite the rules in a way to
fit the model, but it would be quite the endeavour for dubious results
whose validity I wouldn't be so sure of.

But there's a choice-symmetrical parser combinator library in the base
distribution that fits: `ReadP`.  Its interface is almost the same and
I can generate a parser for it as easily as a regex or a full
language:

> instance GrammarRing (ReadP ()) where
>   fromChar c = void (ReadP.char c)
>   a + b = a +++ b
>   a * b = a *> b
> 
> genParser :: Grammar -> ReadP ()
> genParser grammar = fix (toRing grammar) ! 0
>
> genParser2 :: Grammar -> ReadP ()
> genParser2 = genParser
>   . insert 8  (Left [[42],[42,8]])
>   . insert 11 (Left [[42,31],[42,11,31]])
>
> runReadP :: ReadP () -> Message -> Bool
> runReadP p = interpret . filter (null . snd) . readP_to_S p
>   where interpret = \case [] -> False
>                           [_] -> True
>                           _ -> error "Ambiguous parse"

Sure enough, it yields the same results as PCRE.

---

Here's the rest of the code for completeness

> analyzeInput :: Env -> IO ()
> analyzeInput Env{..} = do
>   putStrLn $ show (Map.size grammar) ++ " rules, "
>     ++ show (length messages) ++ " messages"
>   putStrLn $ "Longest message: "
>     ++ show (maximum (length <$> messages)) ++ " characters"
>   putStrLn $ "Total messages' length: "
>     ++ show (sum (length <$> messages)) ++ " characters"
>   putStrLn $ "Raw language size: " ++ show (rawLanguageSize grammar)

> main :: IO ()
> main = do
>   Env{..} <- parseInput <$> readFile "day19.in"
>   analyzeInput Env{..}
> 
>   let language = genLanguage grammar
>   putStrLn $ "Actual language size: " ++ show (Set.size language)
> 
>   performTest messages "Finite language" (`member` language)
>
>   performTest messages "Regex V1" (=~ genRegex grammar)
>   performTest messages "PCRE V2" (=~ genRegex2 grammar)
>
>   performTest messages "ReadP V1" (runReadP (genParser grammar))
>   performTest messages "ReadP V2" (runReadP (genParser2 grammar))

This concludes today's solution.  Hope you liked it!
