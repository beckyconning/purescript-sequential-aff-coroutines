module Test.Array.ST where

import Prelude

import Control.Monad.ST (ST())
import Data.Array (head)
import Control.Monad.Eff (Eff())
import Data.Maybe (Maybe())

import Data.Array.ST

popSTArray :: forall a h eff. STArray h a -> Eff (st :: ST h | eff) (Maybe a)
popSTArray stArr = do
  arr <- freeze stArr
  spliceSTArray stArr 0 1 []
  return $ head arr
