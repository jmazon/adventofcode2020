---
title:       "AoC day 16: Ticket Translation"
author:      "Jean-Baptiste Mazon"
date:        2020-12-16T10:02:57+01:00
tags:        ["advent of ciode", "aoc2020", "haskell"]
description: "List-based parsing and exhaustive search in the list monad"
image:       aoc-2020-16.jpeg
---

[Today, AoC][aoc] starts on a validation problem.  Given a set of
rules, identify the parts of a data set that don't comply.

[aoc]: https://adventofcode.com/2020/day/16

Let's start with the obligatory [literate Haskell][lit] header.

[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day16.lhs

> {-# LANGUAGE RecordWildCards #-}
> import Data.List
> import Data.List.Split

As you can tell from my imports, the parsing isn't going to be too
heavyweight today.  The input file is broken into paragraphs, one for
fields' definitions, one for my ticket and one for other people's
tickets.  The two last sections have a header line.  So here's a
reasonable problem environment definition and high-level parsing.

> data Problem = Pb
>   { fields :: [Field]
>   , myTicket :: Ticket
>   , otherTickets :: [Ticket]
>   }
> parseInput :: String -> Problem
> parseInput input =
>   let [_fields,[_,_myTicket],(_:_otherTickets)] =
>         linesBy null (lines input)
>   in Pb { fields       = parseField  <$> _fields
>         , myTicket     = parseTicket     _myTicket
>         , otherTickets = parseTicket <$> _otherTickets
>         }

The `linesBy` trick may require a word.  It's really not operating on
lines in the usual sense of it, hence the confusion.  Read it as a
form of `sepBy` that doesn't include the separator in the returned
strings and doesn't require termination.  I've already split the input
by lines, so it's aggregating sequences of lines separated by an empty
one—the `null` test.  In other words, paragraphs.

The rest of the parsing is simple recursive descent on `String`s with
list operations.

> type Field = (String,[Range])
> parseField :: String -> Field
> parseField input =
>   let (name,':' : ' ' : ranges) = break (== ':') input
>       [r1,"or",r2] = words ranges
>   in (name,[parseRange r1,parseRange r2])
>
> type Range = (Int,Int)
> parseRange :: String -> Range
> parseRange input =
>   let (lo,'-' : hi) = break (== '-') input
>   in  (read lo,read hi)
> 
> type Ticket = [Int]
> parseTicket :: String -> Ticket
> parseTicket = unfoldr toComma
>   where toComma "" = Nothing
>         toComma x = Just (read l,drop 1 r)
>           where (l,r) = break (== ',') x

Now I need to identify invalid tickets.  A ticket is invalid if it
features a value that can't be mapped to a field.  (Side note: to
simplify reasoning, even though the statement defines in terms of
negatives, I'll only use postive predicates, *i.e.* `isTicketValid`
instead of `isTicketInvalid`.)

> isTicketValid :: [Field] -> Ticket -> Bool
> isTicketValid fields =
>   all (\value -> any (\field -> isFieldValid field value) fields)

And recursive descent to further specify what it means for a value to
fit in a field.

> isFieldValid :: Field -> Int -> Bool
> isFieldValid (_,ranges) value = any (`isRangeValid` value) ranges
>
> isRangeValid :: Range -> Int -> Bool
> isRangeValid (low,high) value = low <= value && value <= high

Let's test it!

<blockquote class="repl">
λ> let Pb{..} = parseInput sample<br>
λ> filter (not . isTicketValid fields) otherTickets<br>
[[40,4,50],[55,2,20],[38,6,12]]
</blockquote>

It reports the same tickets as the statement.  So far, so good.

The puzzle wants us to sum the offending values from the tickets.  So
I'll slightly alter `isTicketValid` to return those.

> ticketInvalidRate :: [Field] -> Ticket -> [Int]
> ticketInvalidRate fields =
>   filter (\value -> all (not . (`isFieldValid` value)) fields)

It was just a matter of replacing `all` with `filter` and inverting
the inner logic (`any` to `all . not`).

Let's verify.

<blockquote class="repl">
λ> map (ticketInvalidRate fields) otherTickets<br>
[[],[4],[55],[12]]
</blockquote>

Those are indeed the offending values.  I can now package this to a
bona-fide function and glean my gold star.

> ticketScanningErrrorRate :: [Field] -> [Ticket] -> Int
> ticketScanningErrrorRate fields tickets =
>   sum $ concatMap (ticketInvalidRate fields) tickets

Part 2 asks for the grunt work: actually identifying which field is
which.

I started with a simple backtracking search, operating on the values
in ticket order.

> orderFields :: [Ticket] -> [Field] -> [[Field]]
> orderFields tickets fields0 = go fields0 (transpose tickets)
>   where
>     go fields (fieldValues:fvs) = do
>       validField <- filter (\f -> all (isFieldValid f) fieldValues)
>                            fields
>       (validField :) <$> go (delete validField fields) fvs
>     go [] [] = pure []

The `transpose` operation is there to convert a list of tickets to a
list of list of values in ticket order, grouping the tickets' values
by their index within a ticket.

For each value set `fieldValues`, it searches the current list of
unplaced fields for one that “fits”.  It then recurses down to
identifying a field for the next ticket index, until all fields are
placed or a dead-end is encountered.

<blockquote class="repl">
λ> let validTickets = filter (isTicketValid fields) otherTickets<br>
λ> map fst <$> orderFields validTickets fields<br>
[["row","class","seat"]]
</blockquote>

It worked!

Well, it's an exhaustive search, it's guaranteed to work.  It's not
guaranteed to work *fast*, though.  Let's implement the puzzle
validation logic, so I can ask the site whether I got it right.

> checkField :: Field -> Int -> Int
> checkField (name,_) value
>   | "departure " `isPrefixOf` name = value
>   | otherwise                      = 1
>
> checkTicket :: [Field] -> Ticket -> Int
> checkTicket fields ticket = product (zipWith checkField fields ticket)

<blockquote class="repl">
λ> let [orderedFields] = orderFields validTickets fields<br>
λ> checkTicket orderedFields myTicket<br>
1
</blockquote>

Is that answer correct?  Hard to say.  Well, it obviously *is* for the
sample where *no* field starts in “departure”, but what about my input
data?

The repl didn't yield an answer in a reasonable time, so I interrupted
it and ran the compiled version.  That one answered correctly under
ten seconds.  So I'm not going to need to optimize that search at all,
in the end.

Here's the end of the program for completeness.

> main :: IO ()
> main = do
>   Pb{..} <- parseInput <$> readFile "day16.in"
>   print $ ticketScanningErrrorRate fields otherTickets
>   let validTickets = filter (null . ticketInvalidRate fields) otherTickets
>       [orderedFields] = orderFields validTickets fields
>   print $ checkTicket fields myTicket

This concludes day 16's solution.  See you around for more advent
Haskell!

---

<small>No footnotes today.  What's *wrong* with me?</small>
