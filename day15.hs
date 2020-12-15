{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad
import Control.Monad.RWS.Lazy
import qualified Data.IntMap.Strict as M

inputs =
  [ [0,1,5,10,3,12,19]
  , [0,3,6]
  ]

game :: [Int] -> [Int]
game starter = snd $ (\m -> evalRWS m () (St 0 undefined M.empty)) $ do
  forM_ starter play
  forever $ do
    lastNumberAge >>= \case
      Nothing -> play 0
      Just age -> play age

lastNumberAge :: MonadState St m => m (Maybe Int)
lastNumberAge = gets stLastAge

play :: (MonadState St m,MonadWriter [Int] m) => Int -> m ()
play n = do
  St{stTurn,stHistory} <- get
  put St { stTurn = stTurn + 1
         , stLastAge = (stTurn -) <$> M.lookup n stHistory
         , stHistory = M.insert n stTurn stHistory
         }
  tell [n]

data St = St { stTurn :: !Int, stLastAge :: Maybe Int, stHistory :: !(M.IntMap Int) }

main = do
  let numbers = inputs !! 0
  print $ game numbers !! (30000000-1)

-- 135250 too high
