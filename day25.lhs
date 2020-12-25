---
title:       "AoC day 25: Combo Breaker"
author:      "Jean-Baptiste Mazon"
date:        2020-12-25T09:14:44+01:00
tags:        ["advent of code", "aoc2020", "haskell"]
description: "Cracking encryption using crazy algorithms and modular arithmetic"
image:       aoc-2020-25.jpeg
---

Christmas is here!  For [today's puzzle][aoc], modular arithmetic
makes its comeback.  A good opportunity to try out using a library to
do it, for once.  This post is a [literate Haskell][lit] program, that
starts with a few imports.

[aoc]: https://adventofcode.com/2020/day/25
[lit]: https://github.com/jmazon/adventofcode2020/blob/master/day25.lhs

> {-# LANGUAGE DataKinds #-}
> import Data.List  (elemIndex)
> import Data.Maybe (fromJust)
> import Data.Modular

Today's modulus is $20201227$.  Probably because -25 and -26 weren't prime.

> type N = Mod Int 20201227

Let's implement the cryptographic protocol, to get the hang of it.

A device has a secret key called “loop size”.  This loop size is the
parameter to a transformation function, which in effect is a power
function.

> newtype LoopSize = LoopSize N
> transform :: N -> LoopSize -> N
> transform subject (LoopSize secret) = subject^(unMod secret)

This secret can be used to generate a public key, by applying the
tranformation to number $7$.

> newtype PublicKey = PublicKey N
> mkPubKey :: LoopSize -> PublicKey
> mkPubKey = PublicKey . transform 7

The encryption key to use between card and door is then defined by
transforming one device's public key with the other's loop size.  By
[mathematical magic][dh][^dh], it happens to produce the same result.
Assuming I'm the card, I'd compute it by using my secret loop size
of 8 to transform the door's public key of $17807724$.

[dh]: https://en.wikipedia.org/wiki/Diffie–Hellman_key_exchange

[^dh]: The easter egg for this puzzle acknowledges Diffie-Hellman,
which was transparent to me, and Merkle, that I don't see just yet.

> encryptionKey :: LoopSize -> PublicKey -> N
> encryptionKey ls (PublicKey pk) = transform pk ls

  > ``` { .repl }
  > λ> encryptionKey (LoopSize 8) (PublicKey 17807724)
  > 14897079
  > ```

Let's verify the other party agrees.

  > ``` { .repl }
  > λ> encryptionKey (LoopSize 11) (PublicKey 5764801)
  > 14897079
  > ```

All good.

Oh, but my input's public keys aren't the same.  How do I extract the
loop size from a public key?

All that needs to be done is invert the operation:
$$ public\ key = 7^{loop\ size} \Leftrightarrow loop\ size = log_7(public\ key) $$

> crackLoopSize :: PublicKey -> LoopSize
> crackLoopSize (PublicKey pk) = LoopSize (logarithm 7 pk)

There are [a few beautiful algorithms][algs] to perform this.
Considering the small modulus we have here, I'll use worthy old brute
force.

[algs]: https://en.wikipedia.org/wiki/Discrete_logarithm#Algorithms

> logarithm :: N -> N -> N
> logarithm base power =
>   toMod $ fromJust $ elemIndex power $ iterate (* base) 1

Here's my wrapper, that also verifies the results are consistent.

> main :: IO ()
> main = do
>   [cardPublicKey,doorPublicKey]
>     <- map (PublicKey . read) . lines <$> readFile "day25.in"
>   print $ encryptionKey (crackLoopSize cardPublicKey) doorPublicKey
>   print $ encryptionKey (crackLoopSize doorPublicKey) cardPublicKey

This concludes today's solution.  See you soon![^more]

[^more]: No more advent of code if you're reading this from the
distant future.  More out-of-sync advent of code if you're from the
present, as I catch up.
