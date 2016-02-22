module Test.Main where

import Control.Apply ((*>))
import Control.Coroutine (($$), ($~), runProcess)
import Control.Coroutine.Aff.Seq (produceSeq)
import Control.Monad.Aff (Aff(), launchAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (Error())
import Data.Array ((:), drop, zipWith, nub)
import Data.Either (Either(..))
import Data.Generic (class Generic)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (uncurry3)
import Prelude (class Show, class Ord, (<<<), ($), (<$>), (==), compose, map, eq, (<*>))
import Test.AsyncCheck (asyncCheck)
import Test.Coroutine (checkGEq, done) as C
import Test.Helpers (stubGet)
import Test.StrongCheck (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen (suchThat)

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

exampleBestThing :: BestThing
exampleBestThing = BestThing { since: "Sliced Bread", name: "PureScript" }

fromNames :: Array String -> Array BestThing
fromNames names = zipWith (\s n -> BestThing { since: s, name: n }) names (drop 1 names)

getBestThing :: forall eff. Array (Either Error BestThing) -> String -> Aff eff BestThing
getBestThing xs = stubGet xs (\since (BestThing obj) -> since == obj.since)

main = do
  log "produceSeq:" *> do
    asyncCheck "Should produce results sequentially." 100 \done atLeastTwoUniqueNames -> do
      let names = runAtLeastTwoAndUnique atLeastTwoUniqueNames
      let bestThings = fromNames $ names.fst : names.snd : names.xs
      let rightBestThings = Right <$> bestThings

      let getter = getBestThing rightBestThings
      let pluckSeq (BestThing obj) = obj.name
      let initialSeq = names.fst

      let produce = produceSeq getter pluckSeq initialSeq

      launchAff $ runProcess (produce $~ (C.checkGEq bestThings) $$ (C.done (liftEff <<< done)))

    -- Should fail when getter provides a left

    -- Should fail when getter fails
