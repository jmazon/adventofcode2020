---
title:       "AoC day 14: Docking Data"
author:      Jean-Baptiste Mazon
date:        2020-12-14T10:51:36+01:00
updated:     2021-01-16T19:27:36+01:00
tags:        ["advent of code", "aoc2020", "haskell", "lens"]
description: "Ditching my performant copypasta for beautiful lenses"
image:       aoc-2020-14.jpeg
---

[Advent of Code's day 14][aoc] presented the rare feature that it made
me punt altogether on sharing code between both parts, and just
copy-paste my code to a new file to alter it.[^blog][^blog_update]

[aoc]: https://adventofcode.com/2020/day/14

[^blog]: It also presents the interesting feature that it's going to
be the first one I publish out-of-order on the blog.  Much XML and
JSON rejoicing awaits me as I hack at the site and its Atom feed to
make everything appear ~~as I want it~~ *as it Should.*

[^blog_update]: No, wait, scratch that, nothing went as it should
have.  It's going to be the *last* one I publish regardless of
ordering, because Doing Things Right had hidden hurdles.

Now the time pressure is off—and I have perfect knowledge of what's in
both parts—let's take a fresh look at it and attempt to do it all at
once.  Starting with the standard [literate Haskell][lit] prelude.

[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day14.lhs

> {-# LANGUAGE RankNTypes #-}
> import           Control.Lens
> import           Control.Lens.Unsound
> import           Control.Monad
> import           Data.Bits
> import           Data.IntMap.Strict (IntMap)
> import qualified Data.IntMap.Strict as Map
> import           Text.Parsec

So what's expected?  We're given a file in the following format:

  >     mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
  >     mem[8] = 11
  >     mem[7] = 101
  >     mem[8] = 0

…and we must interpret it in two different ways:

1. the addresses being strict, the values being modified by the mask
when written
2. the values being strict, the addresses being modified *and
expanded* by the mask when written to

What makes it intrinsically interesting?  Data representation.  The
addressable RAM space is 36 bits, so just a bit beneath
$7×10^10$ slots.  Slots are 36 bits wide as well, so we're looking at
$36×2^36$ bits of addressable data.  That's 288 GiB.  It's more than I
have, so **direct simulation is off**.

My input file is 500 lines, shared between 100 mask changes and
400 memory assignments.  So for part 1, I can safely assume at most
400 of these spaces will contain a value, and I can implement the RAM
storage with a simple `IntMap`.

For part 2, it's more subtle.  The masks may fan out and interfere a
lot with each other, in which case I'd be stuck between the full
288 GiB representation and scratching my head real hard before finding
an adequate representation.  Or they may remain mostly focused and the
part 1 representation still copes.

We can get an idea of the spread by counting the `X`s per mask: each
`X` doubles the number of addresses.

  >     $ perl -lpe '$_=y/X//' day14.in | sort -n | sed -ne '$p'
  >     9

So we're looking at $400×2^9\approx200~000$ 36-bit slots in the worst
case.  That's still acceptable with the `IntMap`.

What can I share between both parts?  Here's a few.

* input data parsing
* RAM representation (but not contents)
* the general linear processing logic

Let's start with the real easy one, **RAM representation**.  I already
decided to go for an `IntMap` in both cases.  36 bits of data need an
`Int64` at least.  I'll just use the standard `Int`, as my system is
64-bit.

> type Address = Int
> type Value   = Int
> type RAM     = IntMap Value

Ok.  Now the general linear **processing logic**.  Squinting a bit, I
have two operations to define: a mask assignment operation and an
addressed memory assignment.

The **mask assignment** is likely going to be different between both
parts, as I may not want to represent the mask the same way, since it
behaves so differently in both.

Then the **addressed memory assignment** operation.  That one might be
more shareable.  Squinting a bit more, it takes an address and a
value, applies a transformation to both, then writes the transformed
value to the transformed address.  The latter not necessarily
remaining singular.

This sounds pretty close to what optics do.  Except I'm no good at
them.

Yet.

*Watch me suffer.*

From what I know, a lens is a getter/setter couple.  Except we don't
ever need to “get” using the masked logic, so I can probably skip the
getter if it makes things too complicated.  The “multiple slot access”
sounds Laarhoven enough to me, but I don't know for sure if it's
compatible with the lens view or if I have to look for some other
glass widget.

\[Cue me scrounging through the documentation\]

  > You can also write to setters that target multiple parts of a structure,

[YES!!!](https://github.com/ekmett/lens/wiki/Examples) That's what I
had in mind!  Now to assemble it properly…

The way to write memory changes each time the mask changes.[^banana]
That's a good sign the setter kind of “is” the mask.  In other words,
I'll be converting each mask assignment to a lens of some sort, and
use it for the subsequent updates.

[^banana]: And [fruit flies like a banana](https://en.wikipedia.org/wiki/Garden-path_sentence).

Let's start with the multiple write, as I suspect it's going to be the
most complex part of it.  Browsing the [lens package][lens], many data
types from `base` seem supported, but unfortunately not the `IntMap` I
had in mind.  There *is* a [`Data.Map.Lens`][map], though, maybe if I
downgrade my type, I can use it?

[lens]: https://hackage.haskell.org/package/lens
[map]: https://hackage.haskell.org/package/lens/docs/Data-Map-Lens.html

  > One of most commonly-asked questions about this package is whether
  > it provides lenses for working with Map. It does, but their uses
  > are perhaps obscured by their genericity. This module exists to
  > provide documentation for them.

Oh.  This may be a fear for the `Data.Map` users, but it's definitely
reassuring for me and my `IntMap`.

On the other hand, it's all about modifying pre-existing values, and
this doesn't apply in my case, as the whole point of using a map is
that I'm trying to avoid inserting the complete set of $2^36$ keys.
So I need to find how to insert before anything else, or this won't
fly.

After some more browsing, I stumble upon [`Control.Lens.At.at`][at],
that seems to do exactly that.  Ok, the building blocks are here,
let's get our hands dirty!

[at]: https://hackage.haskell.org/package/lens/docs/Control-Lens-At.html#v:at

  > ``` { .repl }
  > λ> let ram = Map.fromList [(0,0),(1,1),(2,2)]
  >
  > λ> ram ^.at 1
  > Just 1
  >
  > λ> ram & at 5 ?~ 42
  > fromList [(0,0),(1,1),(2,2),(5,42)]
  > ```

That covers inserting.  Now let's try for multiple values access.
After some more searching, the way to go appears to be `Monoid`s.

  > ``` { .repl }
  > λ> ram ^.. (at 0 <> at 2)
  > [Just 0,Just 2]
  >
  > λ> ram & (at 0 <> at 2) ?~ 42
  > fromList [(0,42),(1,1),(2,2)]
  >
  > λ> ram & (at 0 <> at 2) .~ Just 42
  > fromList [(0,42),(1,1),(2,2)]
  >
  > λ> ram & (at 0 <> at 2) %~ const (Just 42)
  > fromList [(0,42),(1,1),(2,2)]
  > ```

Mmm.  I can manage a multi-value read, but my writes consistently fail
to update more than one value.  Looks like I'm going to have to seek
outside help.

Also, my RAM accesses returning `Nothing` is only an artifact of using
a sparse map in place of a full array.  I ought to adjust for that.

``` Haskell
at' :: Address -> Lens' RAM Value
at' a = \f -> at a (fmap Just . f . fromMaybe 0)
```

Except it triples my runtime.  Possibly because it loses the “strict”
aspect of my `IntMap`.

> at' :: Address -> Lens' RAM Value
> at' a = lens (Map.findWithDefault 0 a) (flip (Map.insert a))

In the meantime, back to basics to try and update multiple slots.
There's this promising function in [`Control.Lens.Setter`][setting] I
can likely put to good use.

[setting]: https://hackage.haskell.org/package/lens/docs/Control-Lens-Setter.html#v:setting

``` Haskell
setting :: ((a -> b) -> s -> t) -> IndexPreservingSetter s t a b
```

  > ``` { .repl }
  > λ> :t Map.alter
  > Map.alter
  >   :: (Maybe a -> Maybe a) -> Map.Key -> Map.IntMap a -> Map.IntMap a
  > 
  > λ> :t setting Map.alter
  > setting Map.alter
  >   :: (Conjoined p, Settable f) =>
  >      p (Maybe a) (f (Maybe a))
  >      -> p Map.Key (f (Map.IntMap a -> Map.IntMap a))
  > 
  > λ> :t set (setting Map.alter)
  > set (setting Map.alter)
  >   :: Maybe a -> Map.Key -> Map.IntMap a -> Map.IntMap a
  >
  > λ> :t set (setting Map.alter) (Just 42) 0 ram
  > set (setting Map.alter) (Just 42) 0 ram :: Num a => Map.IntMap a
  > 
  > λ> set (setting Map.alter) (Just 42) 0 ram
  > fromList [(0,42),(1,1),(2,2)]
  > ```

Damned.  Looks like I reimplemented `at`, but still failed at
multi-update.  Let's try again.

  > ``` { .repl }
  > λ> :t setting
  > setting
  >   :: (Conjoined p, Settable f) =>
  >      ((a -> b) -> s -> t) -> p a (f b) -> p s (f t)
  >
  > λ> :t setting (\f -> Map.alter f 0)
  > setting (\f -> Map.alter f 0)
  >   :: (Conjoined p, Settable f) =>
  >      p (Maybe a) (f (Maybe a)) -> p (Map.IntMap a) (f (Map.IntMap a))
  >
  > λ> :t set (setting (\f -> Map.alter f 0))
  > set (setting (\f -> Map.alter f 0))
  >   :: Maybe a -> Map.IntMap a -> Map.IntMap a
  >
  > λ> :t set (setting (\f -> Map.alter f 0)) (Just 42)
  > set (setting (\f -> Map.alter f 0)) (Just 42)
  >   :: Num a => Map.IntMap a -> Map.IntMap a
  >
  > λ> :t set (setting (\f -> Map.alter f 0)) (Just 42) ram
  > set (setting (\f -> Map.alter f 0)) (Just 42) ram
  >   :: Num a => Map.IntMap a
  >
  > λ> set (setting (\f -> Map.alter f 0)) (Just 42) ram
  > fromList [(0,42),(1,1),(2,2)]
  >
  > λ> set (setting (\f -> Map.alter f 0 . Map.alter f 2)) (Just 42) ram
  > fromList [(0,42),(1,1),(2,42)]
  > ```

YES!!![^blocker]

[^blocker]: ~~This has been, and remains to date, my biggest blocker
before publication.~~ I've tried to make a traversal of it in many
Many different ways ~~and failed at all.  Relieve me by telling me
which foundational law doing it would go against.  Or ridicule me by
solving it in two combinators, that would still be nice~~.  Ok this
solved itself in the end.  Whew.

Let's package that into a convenient function while I still remember
how I did it.

``` Haskell
ats :: Foldable l => l Int
    -> Setter' (Map.IntMap a) (Maybe a)
ats is = setting $ \f -> appEndo (foldMap (Endo . Map.alter f) is)
```

Verification before moving on…

  > ``` { .repl }
  > λ> ram & ats [2..4] ?~ 42
  > fromList [(0,0),(1,1),(2,42),(3,42),(4,42)]
  > ```

No, wait.  The actual “correct” way has just revealed itself to me,
after I've read most of the `lens` documentation 15× I just stumbled
upon `Control.Lens.Unsound` at last.  And it has all the missing bits
I wanted.  Unsound but correct.  At last!

``` Haskell
ats :: [Address] -> Traversal' RAM Value
ats = foldr1 adjoin . map at'
```

Except it doesn't work.  I'll spare you the compilation error and some
of the detail.  It took weeks of trying and [a StackOverflow
question][so] to get it upright.[^blocker2] But I have it now, I'm
holding on to it!

[so]: https://stackoverflow.com/a/65722746/12274
[^blocker2]: And *that* has been the greatest blocker of them all.

> ats :: Foldable l => l Int -> Traversal' RAM Value
> ats = runTraversal .
>   foldr (\e t -> Traversal $ at' e `adjoin` runTraversal t)
>         (Traversal ignored)

Moving on.  I'd like to perform the changes using a **single lens
operation**.  That <del>`at`</del> <ins>`ats`</ins> function seems
like the right abstraction, except it builds a <del>`Lens`</del>
<ins>`Traversal`</ins> when I only need a `Setter`.  ~~As long as I'm
writing this on my own, I'd rather skip on the (keyboard)
typing.[^skip]~~ In that vein, I'd like a setter that takes an address
as its input, and focuses on the addresses that make sense in the
current context.

[^skip]: That, and it couldn't really be a lens anyway, seeing as it
addresses multiple values.  ~~I *am* curious as to whether there's a
more direct way of combining all of this.  If you know of one, I'd
like to hear from you!~~ I'd always like to hear from you, but this
part is solved now I got the `Traversal` working.

The list of addresses to write can't be known at the time the mask is
parsed, as it depends on the given address.  What *is* known is the
function to convert an address to a list of addresses.  So I can
construct my setter at parse time, by composing my `ats` combinator
with that function.  Problem solved!  Implementation deferred, as I'd
still like to share the parsing with part 1.

Let's study the specifics of part 1.  Here we have direct addressing,
so `at` is indeed the perfect match.  And I'd need to hack together a
setter that modifies the value being written.  This shouldn't be too
much of a problem, using `setting` as before, this time without
changing the level of the setter's focus.

Well, well.  I do have everything I need as far as lens operations go.
Now to put the icing on the cake, I'd like to reduce the processing to
a single operation per memory write input instruction.  For both parts
at once.

Speaking FP jargon, I'd want an endomorphism on a product state of a
v1-style RAMfile and a v2-style RAMfile.  That I'd build from
endomorphisms on the two separate RAMs.

This sounds close to arrow combinators, and indeed the `overA` one
hits pretty close.  But not perfect.  Next hunch is that it's yet
another multisetter, this time deferring to two inner setters,
providing them with the same value.  So maybe something more like
applying a value to two setters in a reader (`->`) monad.

Let's try to type and implement it.

> pairSetter :: Setter' a b -> Setter' a b -> ReifiedSetter' (a,a) b
> pairSetter s1 s2 = Setter $ setting $ \f (x,y) -> (x & s1 %~ f,y & s2 %~ f)

Wow.  That compiled.  Let's try it, just to be sure.

  > ``` { .repl }
  > λ> :t pairSetter (at 0) (at 100)
  > pairSetter (at 0) (at 100)
  >   :: (Settable f, At a, Num (Index a)) =>
  >      (Maybe (IxValue a) -> f (Maybe (IxValue a))) -> (a, a) -> f (a, a)
  >
  > λ> :t pairSetter (at 0) (at 100) ?~ 42
  > pairSetter (at 0) (at 100) ?~ 42
  >   :: (Num (IxValue a), At a, Num (Index a)) => (a, a) -> (a, a)
  >
  > λ> (ram,ram) & pairSetter (at 0) (at 100) ?~ 42
  > (fromList [(0,42),(1,1),(2,2)],fromList [(0,0),(1,1),(2,2),(100,42)])
  > ```

Looks all right.

After I found `adjoin` in the unsound subpackage, I realize I could
have spared some work with `lensProduct`.  But at least this time, I
got what I wanted independently, so I'll keep it and not make this
post even more confusing.

---

I've exhausted my entire “what-if” list. There's nothing left to do
but **implement**.

To construct the setter, I'll convert the parsed mask to two masks for
part 1 and more for part 2.

> type Write = Address -> ReifiedSetter' (RAM,RAM) Value
>
> -- | An assignment is a setter on an address.
> address :: (And,Or) -> (And,[Or]) -> Write
> address p1 p2 a = Setter $
>   runSetter (pairSetter (addrV1 p1 a) (addrV2 p2 a))

~~For some reason beyond my understanding by now, that previous
definition won't compile as `liftA2 pairSetter addrV1 addrV2`.  More
lens magic I'm not worthy of understanding just yet.  Oh well.~~
Probably more `Setter` vs `ReifiedSetter` woes.  I'm at peace with
those now.

> -- | V1 addresses are used directly; values get applied the two
> -- masks before writing.  Mask order is irrelevant as the bitsets
> -- they operate on are disjoint.
> addrV1 :: (And,Or) -> Address -> Lens' RAM Value
> addrV1 (And andMask,Or orMask) a = \f ->
>   at' a $ fmap ((.|. orMask) . (.&. andMask)) . f
>
> -- | V2 addresses get masked and multiplied per mask; values are
> -- | used directly.  Masking order *is* relevant here as the bitsets
> -- | overlap totally.
> addrV2 :: (And,[Or]) -> Address -> Traversal' RAM Value
> addrV2 (And andMask,orMasks) a = ats
>   [ a .&. andMask .|. orMask | Or orMask <- orMasks ]

I'll perform all operations directly from the parser, using `Parsec`'s
state to store the current `Write`.  That state will be updated each
time a mask assignment is parsed.

> type Parser = Parsec String Write

With that state, the program will thread a dual RAM as the monadic
return value of each instruction.

> program :: Parser (Int,Int)
> program =
>   bimap sum sum . foldl (&) (Map.empty,Map.empty) <$>
>   many instruction <* eof

So each instruction is expected to return a dual RAM modifier
function.

> instruction :: Parser ((RAM,RAM) -> (RAM,RAM))
> instruction = (setMask <|> setMem) <* endOfLine <?> "instruction"

Setting a mask doesn't alter the RAM yet, so it returns `id`.

> setMask :: Parser ((RAM,RAM) -> (RAM,RAM))
> setMask = try (string "mask = ") *> (mask >>= putState) *> pure id

The mask is parsed bit by bit.

> mask :: Parser Write
> mask = toWrite . mconcat <$> traverse maskBit [35,34..0]
>   where
>     toWrite (masks1,(and2,ors2)) =
>       address masks1 (and2,foldM (map . mappend) mempty ors2)

Each bit contributes some update to the writer's settings.

* for part 1, `X`s are ignored; known digits contribute either to the
  “and” mask (zeros) or the “or” mask (ones)
* for part 2, `0`s are ignored; `1`s contribute to the “or” masks (all
  of them); `X`s contribute to the “or” masks as a duplication: both
  as a `0` and a `1` bit.  So I'll use the monadic list transformation
  to aggregate “or diffs”: lists of masks to or to the ones I already
  had.  A two-element list would duplicate; a one-element list is a
  form of `fmap (.|.)`.  A zero-element list (`[[]]`) would clear all
  masks, but I'm not going to use that: I'll use an absence of list
  (`[]`, *i.e.* `mempty`) instead to do nothing.  The monadic
  expansion is performed in the `mask` function above.

> maskBit :: Int -> Parser ((And,Or),(And,[[Or]]))
> maskBit i =
>       (         mempty      , (And (bit' i),[[Or 0,Or (bit i)]])) <$ char 'X'
>   <|> ((And (bit' i),mempty),           mempty                  ) <$ char '0'
>   <|> ((mempty,  Or (bit i)), (   mempty   ,  [[Or (bit i)]]   )) <$ char '1'
>   <?> "maskBit"

I like how `mempty` serves as a perfectly acceptable substitute for
`(mempty,mempty)`.

With the writer in the parser's state, I can now write the memory
assignment operation's implementation “naturally”.

> setMem :: Parser ((RAM,RAM) -> (RAM,RAM))
> setMem = do
>   void $ string "mem["
>   addr <- number
>   void $ string "] = "
>   value <- number

---

It is now my greatest pleasure, ladies and gentlemen, to present to
you the following next two lines.  This entire post's *raison d'être*,
seeking, mumbling, exploring, cursing and overengineering, is to put
myself in such a position to be able to write them as such.

>   write <- getState
>   pure $ runSetter (write addr) .~ value

---

The main wrapper uses a dummy initial mask.  I'd normally use a call
to `error` instead, but `Parsec`'s state is unconditionally strict,
and it would bring yet more complexity to wrap it lazy again, and for
little gain.  Just don't feed the program inputs that don't start with
a mask assignment, ok?

> main :: IO ()
> main =
>   print . runParser program (address mempty mempty) "source code"
>     =<< readFile "day14.in"

The rest is helpers and support.

> newtype And = And Int
> instance Semigroup And where And a <> And b = And (a .&. b)
> instance Monoid    And where mempty = And (complement 0)
>
> newtype Or = Or Int
> instance Semigroup Or where Or a <> Or b = Or (a .|. b)
> instance Monoid    Or where mempty = Or 0
>
> bit' :: Bits b => Int -> b
> bit' = complement . bit
>
> number :: Parser Int
> number = read <$> many1 digit <?> "number"

This concludes “today”'s solution.  A month into the making.

It's brutally inefficient—it solved my case in just over four
minutes—yet it worked on the first try.  And I got to understand way
more than I set up to about `lens`es' nitty-gritty.

It also concludes the series, being the twenty-fifth solution I
*finally* get to publish.  I'll write a recap as time permits.

I hope you enjoyed reading about my lenses journey.  See you soon!
