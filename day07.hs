{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards,TypeApplications,ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving,DeriveFunctor,DeriveFoldable #-}
import Data.Char (isSpace)
import Data.Function (on)
import Text.Parsec
import Data.Semigroup (stimesMonoid,Any(..),Sum(..))
import Control.Applicative (liftA2,Const(..))
import qualified Data.Set as S
import Data.Bifunctor
import Data.Functor.Identity (Identity(..))
import Data.Functor.Foldable
import Data.Functor.Classes

type Qualifier = String
type ColorName = String
data Color = Adj Qualifier ColorName deriving (Show,Eq,Ord)

data Times a = Times { timesMult :: Int
                     , timesOperand :: a
                     }
             deriving (Functor,Show,Eq,Ord)
instance Foldable Times where foldMap f (Times a b) = stimesMonoid a (f b)
instance Show1 Times where
  liftShowsPrec sp sl p (Times m o) =
    showParen (p > 10) $
    showString "Times " .
    showsPrec 11 m .
    showChar ' ' .
    sp 11 o

type Entry = BagF Color Color
data BagF a b = BagF { bagColor :: a
                     , bagContents :: [Times b]
                     }
              deriving (Show,Functor,Foldable)
instance Eq a => Eq (BagF a f) where (==) = (==) `on` bagColor
instance Ord a => Ord (BagF a f) where compare = compare `on` bagColor
instance Show a => Show1 (BagF a) where
  liftShowsPrec sp sl p BagF{..}  =
    showString "BagF { bagColor = " .
    shows bagColor .
    showString ", bagContents = " .
    liftShowList sp sl bagContents .
    showString " }"

type Graph = [Vertex Color]
newtype Vertex a = Vertex { getVertex :: Fix (BagF a) } deriving Show

instance Foldable Vertex where
  foldMap :: forall a m. Monoid m => (a -> m) -> Vertex a -> m
  foldMap f (Vertex fb) = cata c fb where
    c :: Base (Fix (BagF a)) m -> m
    c (BagF a tms) = f a <> foldMap fold tms

main = do
  Right entries <- parse parser "in7.in" <$> readFile "in7.in"
  let bags = -- map (\(BagF c es) -> Vertex $ Fix $ BagF c (fmap (getVertex . index) <$> es)) entries :: Graph
        map (Vertex . Fix . fmap (getVertex . index)) entries :: Graph
        where index c = head (filter ((== c) . bagColor . unfix . getVertex) bags)
              index :: Color -> Vertex Color
      shinyGold = head $ filter ((== Adj "shiny" "gold") . bagColor . unfix . getVertex) bags :: Vertex Color
  putStrLn "Remember to subtract 1: the shiny gold bag is never counted in."
  print (length bags)
  print $ length $
    filter (getAny . foldMap (Any . (== bagColor (unfix (getVertex shinyGold))))) bags
--  print $ foldMap (const (Sum 1)) shinyGold

-- Post-game analysis: there are actually no loops!

type Parser = Parsec String ()

parser :: Parser [Entry]
parser = entry `endBy` (string ".\n") <* eof

entry :: Parser Entry
entry =
  BagF <$> bag
       <*  string "contain "
       <*> contents

bag :: Parser Color
bag =
  Adj <$> (word <?> "qualifier")
      <*> (word <?> "color")
      <*  string "bag"
      <*  optional (char 's')
      <*  spaces
      <?> "bag"

contents :: Parser [Times Color]
contents =
  [] <$ string "no other bags"
  <|> liftA2 Times num bag `sepBy1` string ", "
  <?> "contents"

num :: Parser Int
num = read <$> many1 digit <* spaces

word :: Parser String
word = many1 (satisfy (not . isSpace)) <* spaces
