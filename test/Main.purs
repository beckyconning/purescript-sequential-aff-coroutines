module Test.Main where

import Prelude

import Debug.Trace
import Control.Apply ((*>))
import Control.Monad.Aff (Aff(), launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (log)
import Control.Monad.ST (ST())
import Data.Array ((:), head, length, drop, zipWith, nub, sort)
import Data.Array.ST (freeze, thaw)
import Data.Either (Either(..), either)
import Data.Functor (($>))
import Data.Foldable (foldl)
import Data.Generic (Generic, gEq, gShow)
import Data.Maybe (maybe, Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (uncurry3)

import Test.AsyncCheck (asyncCheck)
import qualified Test.Coroutine (collect, take, checkGEq, done) as C
import Test.Helpers (stubGet)
import Test.StrongCheck (Arbitrary, arbitrary)
import Test.StrongCheck.Generic (gArbitrary)
import Test.StrongCheck.Gen (suchThat)

import Control.Coroutine

import Control.Coroutine.Aff.Seq

newtype BestThing = BestThing { since :: String, name :: String }

derive instance genericBestThing :: Generic BestThing

instance arbBestThing :: Arbitrary BestThing where
  arbitrary = BestThing <$> ({ since: _, name: _ } <$> arbitrary <*> arbitrary)

newtype AtLeastTwoAndUnique a = AtLeastTwoAndUnique (Tuple (Tuple a a) (Array a))

runAtLeastTwoAndUnique :: forall a. AtLeastTwoAndUnique a -> { fst :: a, snd :: a, xs :: Array a }
runAtLeastTwoAndUnique (AtLeastTwoAndUnique tuple) = (uncurry3 { fst: _, snd: _, xs: _ }) tuple

instance arbAtLeastTwoAndUnique :: (Arbitrary a, Ord a, Show a) => Arbitrary (AtLeastTwoAndUnique a) where
  arbitrary = AtLeastTwoAndUnique `map` (arbitrary `suchThat` (unique `compose` toArray))
    where
    unique xs = nub xs `eq` xs
    toArray (Tuple (Tuple x1 x2) xs) = x1 : x2 : xs

exampleBestThing = BestThing { since: "Sliced Bread", name: "PureScript" }

fromNames :: Array String -> Array BestThing
fromNames names = zipWith (\s n -> BestThing { since: s, name: n }) names (drop 1 names)

getBestThing :: forall eff. Array BestThing -> String -> Aff eff (Either Unit BestThing)
getBestThing xs = stubGet (Right <$> xs) (\since (BestThing obj) -> since == obj.since)

main = do
  log "produceSeq:" *> do
    asyncCheck "Should produce results sequentially." 100 \done atLeastTwoUniqueNames -> do
      let names = runAtLeastTwoAndUnique atLeastTwoUniqueNames
      let bestThings = fromNames $ names.fst : names.snd : names.xs

      let getter = getBestThing bestThings
      let pluckSeq (BestThing obj) = obj.name
      let initialSeq = names.fst

      let produce = produceSeq getter pluckSeq initialSeq

      launchAff $ runProcess (produce $~ (C.checkGEq bestThings) $$ (C.done (liftEff <<< done)))

    -- Should fail when getter provides a left

    -- Should fail when getter fails
