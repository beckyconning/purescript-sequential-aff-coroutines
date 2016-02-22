module Test.AsyncCheck where

import Control.Apply ((*>))
import Control.Monad.Eff (foreachE, Eff())
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (newRef, REF, readRef, modifyRef)
import Control.Monad.Trampoline (runTrampoline)
import Data.Array ((:), length)
import Data.Foldable (all)
import Prelude (Unit, ($), unit, return, (==), bind, (++))
import Test.Process (exit, EXIT, logGreen, logRed, COLORLOG)
import Test.StrongCheck (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen (sample)

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

