module Test.AsyncCheck where

import Prelude

import Test.Process (exit, EXIT(), logGreen, logRed, COLORLOG())
import Data.Foldable (all)
import Data.Array ((:), length)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (foreachE, Eff())
import Control.Monad.Eff.Console (log, CONSOLE())
import Control.Monad.Eff.Random (RANDOM())
import Control.Monad.Eff.Ref (newRef, REF(), readRef, modifyRef)
import Control.Monad.Trans (lift)
import Control.Apply ((*>))
import Control.Monad.Trampoline (runTrampoline)

import Test.StrongCheck.Gen
import Test.StrongCheck

type AsyncCheckEff eff =
  Eff (exit :: EXIT, colorLog:: COLORLOG, console :: CONSOLE, ref :: REF, random :: RANDOM | eff) Unit

type AsyncCheckable a eff =
  (Boolean -> AsyncCheckEff eff) -> a -> AsyncCheckEff eff

asyncCheck :: forall a eff. (Arbitrary a) =>
  String -> Int -> AsyncCheckable a eff -> AsyncCheckEff eff
asyncCheck s n f = do
  refTestResults <- newRef []
  let done result = do
        modifyRef refTestResults (result :)
        testResults <- readRef refTestResults
        if (length testResults == n) then allDone testResults else return unit
  foreachE (runTrampoline $ sample n arbitrary) (f done)
    where
    allDone xs = case ((all (== true) xs)) of
      true -> logGreen $ "  ✔︎ " ++ s
      false -> logRed ("  ✘ " ++ s) *> exit 1

