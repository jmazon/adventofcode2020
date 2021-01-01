---
title:       "AoC day 7: Handy Haversacks"
author:      "Jean-Baptiste Mazon"
date:        "2020-12-07T08:31:16+01:00"
updated:     "2021-01-01T14:08:16+01:00"
tags:        ["advent of code", aoc2020, haskell]
description: "Counting bags with anamorphisms and catamorphisms"
image:       aoc-2020-07.jpeg
---

Fair warning: this may not be my longest code of the year, but I do
consider it the most overengineered.  [Today's challenge][aoc] is to
extract various metrology information about a distinguished bag in a
graph of bags.  I'll proceed in [literate Haskell][lit], with a few
extensions and imports.

[aoc]: https://adventofcode.com/2020/day/7
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day07.lhs

> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE TypeFamilies  #-}
>
> import Control.Applicative (liftA2)
> import Data.Char           (isSpace)
> import Data.Maybe          (fromMaybe)
> import Data.Semigroup      (stimesMonoid,Any(..),Sum(..))
> import Data.Foldable       (find,fold)
> import Data.Functor.Foldable hiding (fold)
> import Text.Parsec
>
> type Parser = Parsec String ()

The puzzle today defines a directed graph with labeled edges.  Nodes
are bags, labeled by color; edges define how many bags of each color a
bag contains, directed from container to contained.  We're always
going to consider the graph from the perspective of a distinguished
node: the shiny gold bag.

It follows from the puzzle assignments there cannot be a circuit in
the subgraph generated from the shiny gold bag, as that would mean an
infinite number of bags for part 2, which doesn't quite fit the
format.  I don't see anything preventing circuits in the rest of the
graph: it wouldn't invalidate the reasoning for the part of the graph
examined in part 1, and if there's a disconnected subgraph in there
somewhere, so be it.[^ex]

[^ex]: For example, “an oily blue bag contains 1 true pearly bag; a
true pearly bag contains 1 oily blue bag.”  It doesn't invalidate the
universe to have those rules in the regulations.  It merely makes it
very hard for travellers to actually travel with them if they don't
have an infinite supply at hand.  But they can still travel with shiny
gold bags, so all hope is not lost.

I don't recall encountering any in my input though.

Here's the statement sample's graph to make things clearer.

![](/images/aoc-2020-07-graph.svg)

Part 1 asks to *count* the number of bags “left” of shiny gold.
Part 2 asks to *sum* the bags counts “right” of shiny gold, weighted
by the edges' labels.

All in all, part 2 is actually quite straightforward.  It's part 1
that requires a bit of thinking before diving in.  The sane thing to
do on the general case is to invert the edges' direction and
[DFS][dfs].  But the graph is quite small—mine is 594 nodes broad.  So
it's IMHO easier to just DFS over every node.  In the absence of
loops, that search doesn't even need memory: the graph can be
considered a tree with no ill effect other than some wasted time.  So
it's easier to just tree-fold over every node.  This actually extends
to part 2.

[dfs]: https://en.wikipedia.org/wiki/Depth-first_search

Ok, let's get to it.

My complication of the day, by personal choice, will be to implement
the folds using [`recursion-schemes`][rec].  So I'll need a base
functor.

[rec]: https://hackage.haskell.org/package/recursion-schemes

> data VertexF a b = VertexF
>   { vtxLabel :: a
>   , vtxEdges :: [Times b]
>   }
>   deriving (Show,Functor)

My edge labeling is a simple integer, with multiplicative semantics,
so I implement with a `Times` type and the appropriate fold.

> data Times a = Times
>   { timesFactor  :: Int
>   , timesOperand :: a
>   }
>   deriving (Functor,Show,Eq,Ord)
> instance Foldable Times where foldMap f (Times a b) = stimesMonoid a (f b)

The input is given as a series of nodes, one per line, with all out
edges.  This happens to fit the graph's base functor quite well!  I
can store it in an intermediate representation using said functor
directly.

> type Entry = VertexF Color Color

Now to parse.  This input is in the middle of that gray area where I
could use split/list combinators or actual parsers.  I'll settle for
`Parsec` this time.

> parser :: Parser [Entry]
> parser = entry `endBy` (string ".\n") <* eof

As said earlier, each input line is an entry with:

* a node: the container bag
* edges: the contained bags with the matching weights

> entry :: Parser Entry
> entry =
>   VertexF <$> bag
>           <*  string "contain "
>           <*> contents

As far as this puzzle is concerned, a bag is isomorphic to its color.

> type Qualifier = String
> type ColorName = String
> data Color = Adj Qualifier ColorName deriving (Show,Eq,Ord)
>
> bag :: Parser Color
> bag =
>   Adj <$> (word <?> "qualifier")
>       <*> (word <?> "color")
>       <*  string "bag"
>       <*  optional (char 's')
>       <*  spaces
>       <?> "bag"

The “contained” part of the line has a bit of special casing for when
it's empty.

> contents :: Parser [Times Color]
> contents =
>   [] <$ string "no other bags"
>   <|> liftA2 Times num bag `sepBy1` string ", "
>   <?> "contents"

And here are two helpers to conclude the parser.

> num :: Parser Int
> num = read <$> many1 digit <* spaces
> 
> word :: Parser String
> word = many1 (satisfy (not . isSpace)) <* spaces

And an actual wrapper…

> parseNodes :: String -> [Entry]
> parseNodes = either (error . show) id . parse parser "puzzle input"

So I'm now able to parse the input to a list of “entries”: colors
representing bags pointing to other colors.  That's not a graph yet, I
still need to tie the loop from a node to the next.

I don't have a workable notion of a root here, so I'll just represent
the graph as a list of vertices.

> type Graph = [Vertex]
> type Vertex = Fix (VertexF Color)

And start implementing.

> nodesToGraph :: [Entry] -> Graph
> nodesToGraph entries = graph where

I can keep the list structure and convert entry-wise.

>   graph = map nodeToVertex entries

Building the graph from a seed node is an anamorphism.

>   nodeToVertex :: VertexF Color Color -> Vertex
>   nodeToVertex = ana expandNode

The seed is already in `VertexF` form, so I can use the functor
instance to convert it in-place.

>   expandNode :: VertexF Color Color -> VertexF Color (VertexF Color Color)
>   expandNode = fmap lookupNode

Implenting it by fetching a `VertexF Color Color` from where I have
them: the `entries` list.

>   lookupNode :: Color -> VertexF Color Color
>   lookupNode color =
>     fromMaybe (error "Edge to non-existant node") $
>     find ((== color) . vtxLabel) entries

This works.  It's probably not smart enough to deduplicate the
subforests on its own, though.  Not that it would be needed to solve
the problem.

> solve :: Graph -> [Int]
> solve g = [part1,part2] where
>   shinyGold = Adj "shiny" "gold"
>   part1 = length (filter (shinyGold `elem'`) g) - 1
>   part2 = length' (lookupVertex shinyGold g) - 1

Minus one because the folds are inclusive, *i.e.* they count lax
containment, whereas the problem asks for strict.

Oh, and I need to specialize the folds for my structure because for
some reason there's no automatic `Foldable` instance.^[I'm not exactly
sure I'm doing all of this right.]

>   foldVertexF f (VertexF l rec) = f l <> foldMap fold rec
>   elem' e = getAny . cata (foldVertexF (Any . (e ==)))
>   length' = getSum . cata (foldVertexF (const (Sum 1)))

I use this helper; I've extracted it to toplevel because it'll come in
handy later on.

> lookupVertex :: Color -> Graph -> Vertex
> lookupVertex v =
>   fromMaybe (error "Vertex not found") .
>   find ((== v) . vtxLabel . unfix)

At any rate, it works.

  > ``` { .repl }
  > λ> solve $ nodesToGraph $ parseNodes sample
  > (4,32)
  > ```

Here's my `main` so I can run it on my puzzle input.

> main :: IO ()
> main =
>   mapM_ print . solve . nodesToGraph' . parseNodes =<< readFile "day07.in"

A bit under a second to solve for my puzzle input.  So yes it works,
but still rather inefficient.  The obvious cause being the fact I
treat a dense graph as a forest, taking no advantage of the fact some
nodes are encountered numerous times from different paths.

In this post, I'll only push a little further: constructing the graph
with sharing.

Recall that I'm constructing the graph vertex by vertex by
anamorphism, where each vertex is recursively expanded by generating
its own subforest.  For example where a shiny gold bag contains dark
olive bags, I'd construct a dark olive node as a descendant to the
shiny gold bag, and an additional node when encountered independently
in the bag list.

To share the constructed vertices, I'll store them in a list and fill
in the subtrees by looking up in there instead of the original entries
list.

The global function signature remains the same.

> nodesToGraph' :: [Entry] -> Graph
> nodesToGraph' entries = graph where

The graph is still a list of vertices.  I can still construct it by
mapping over the entries.

>   graph = map nodeToVertex entries

This time I won't use the anamorphism; I'll use the functor directly.

>   nodeToVertex :: Entry -> Vertex
>   nodeToVertex = Fix . fmap (`lookupVertex` graph)

Doing so halves the total runtime.  To make any more progress, I'd
have to implement the measurements as actual graph operations instead
of tree folds.  Which is pointless, since this problem is already
solved.

But that's a subject for another post.

This concludes this installment of today's solution.  See you soon!
