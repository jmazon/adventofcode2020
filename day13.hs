import Prelude
import Control.Arrow
import Data.List
import Data.Maybe
import Text.Read
import Data.Ord

-- parse :: String -> Maybe (Maybe Int,String)
parse input = (read start,unfoldr go schedule) where
  [start,schedule] = lines input
  go"" = Nothing
  go s = Just $ readMaybe *** drop 1 $ (break (== ',') s)

main :: IO ()
main = do
  (start,schedule) <- parse <$> readFile "in13.in"
  print $ uncurry (*) $ minimumBy (comparing snd) $ map (id &&& mod (negate start)) $ catMaybes schedule
  let remainders = catMaybes $ zipWith (\i mb -> fmap (flip (,) i) mb) [0,-1..] schedule
      t = uncurry chinese (unzip remainders)
  print t

chinese :: [Int] -> [Int] -> Int
chinese divisors remainders = x `mod` n where
  n = product divisors
  es = map (\ni -> let ni_ = n `div` ni in  snd (egcd ni ni_) * ni_) divisors
  x = sum (zipWith (*) remainders es)

egcd :: Int -> Int -> (Int,Int)
egcd 1 0 = (1,0)
egcd _ 0 = error "egcd: Not coprime"
egcd a b = let (u,v) = egcd b (a `mod` b)
           in (v,u-a `div` b * v)
