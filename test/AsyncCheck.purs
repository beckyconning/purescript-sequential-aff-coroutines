module Test.AsyncCheck where

import Prelude

import Data.Functor (($>))
import Data.Maybe (maybe, Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (all, Foldable)
import Data.Traversable (sequence)
import Data.List (length, toList, List(..))
import Control.Monad.Aff (Aff(), launchAff, later)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Eff (foreachE, Eff())
import Control.Monad.Eff.Random (RANDOM(), random)
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Trans (lift)
import Control.Monad.Trampoline (runTrampoline)

import Control.Coroutine
import Control.Coroutine.Aff

import Test.Coroutine (collect, take)
import Test.StrongCheck.Gen
import Test.StrongCheck

defState :: Seed -> GenState
defState s = (GenState {seed: s, size: 10})

throwOnFirstFailure :: forall f. (Foldable f) => Int -> f Result -> QC Unit
throwOnFirstFailure n fr = throwOnFirstFailure' n (toList fr)
  where
  throwOnFirstFailure' :: Int -> List Result -> QC Unit
  throwOnFirstFailure' _ Nil = pure unit
  throwOnFirstFailure' n (Cons (Failed msg) _) = throwException $ error $ "Test " <> show n <> " failed: \n" <> msg
  throwOnFirstFailure' n (Cons _ rest) = throwOnFirstFailure (n + 1) rest

countSuccesses :: forall f. (Foldable f) => f Result -> Int
countSuccesses fa = countSuccesses' 0 (toList fa)
  where
  countSuccesses' acc Nil = acc
  countSuccesses' acc (Cons Success rest) = countSuccesses' (acc + 1) rest
  countSuccesses' acc (Cons _ rest) = countSuccesses' acc rest

-- affCheck \x y -> makeAff \_ done ->

affCheck :: forall a. (Testable a) => (a -> Aff _ Result) -> Eff _ Unit
affCheck f = do
  seed <- random
  sequence $ runTrampoline $ sample' 100 (defState seed) (affTest f)
