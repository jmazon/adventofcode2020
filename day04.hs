{-# LANGUAGE LambdaCase #-}
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List.Split
import Data.Maybe

validate :: String -> Bool
validate passport = all (`valid` fields) ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
  where fields = map field (words passport)
        valid f kvs = fromMaybe False (lookup f kvs >>= validate f)
        validate :: String -> String -> Maybe Bool
        validate "byr" = digits 4 1920 2002
        validate "iyr" = digits 4 2010 2020
        validate "eyr" = digits 4 2020 2030
        validate "hgt" = \s -> let (ds,ls) = break isLetter s in
          case ls of "cm" -> digits 3 150 193 ds
                     "in" -> digits 2 59 76 ds
                     _ -> mzero
        validate "hcl" = \case '#':xs@[_,_,_,_,_,_] -> pure (all isHexDigit xs)
                               _ -> mzero
        validate "ecl" = pure . (`elem` ["amb","blu","brn","gry","grn","hzl","oth"])
        validate "pid" = \s -> pure $ length s == 9 && all isDigit s
        validate x = error x
        digits l f t ds = pure $ length ds == l && read ds >= f && read ds <= t

field :: String -> (String,String)
field entry = second tail $ break (== ':') entry

main = do
  ps <- split (onSublist "\n\n") <$> readFile "in4.in"
  print $ length $ filter validate ps
  return ()
