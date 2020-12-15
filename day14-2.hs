{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.RWS.Strict
import Data.Bits
import Data.List
import qualified Data.IntMap.Strict as M
import Text.Parsec

type Parser = Parsec String ()
type M = RWS () (Dual (M.IntMap Int)) (Int -> [Int])

instruction :: Parser (M ())
instruction = (setMask <|> setMem <?> "instruction") <* eof

setMask :: Parser (M ())
setMask = try (string "mask = ") *> (put <$> mask)

mask :: Parser (Int -> [Int])
mask = foldl1' (>=>) . zipWith (flip ($)) [0..] . reverse <$> replicateM 36 maskBit

maskBit :: Parser (Int -> Int -> [Int])
maskBit =
  char 'X' *> pure (\b i -> [clearBit i b,setBit i b])
  <|> char '0' *> pure (\b i -> [i])
  <|> char '1' *> pure (\b i -> [setBit i b])
  <?> "maskBit"

setMem ::  Parser (M ())
setMem = do
  void $ string "mem["
  addr <- number
  void $ string "] = "
  value <- number
  pure $ do
    curMask <- get
    forM_ (curMask addr) $ \ea -> tell $ Dual $ M.singleton ea value

number :: Parser Int
number = read <$> many1 digit <?> "number" :: Parser Int
  
main :: IO ()
main = do
  text <- lines <$> readFile "in14.in"
  let res = mapM (parse instruction "source code") text
  case res of
    Left err -> error (show err)
    Right code -> do
      let (_,ram) = execRWS (sequence_ code) () (error "No initial mask")
      print $ sum (getDual ram)
      pure ()
