module Test.Main where

import Prelude

import Control.Apply ((*>))
import Control.Coroutine
import Control.Coroutine.Aff
import Control.Monad
import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Rec.Class
import Control.Monad.Free.Trans
import Data.Tuple
import Control.Monad.ST
import Data.Array hiding (filter)
import Data.Array.ST
import Data.Either
import Data.Functor (($>))
import Data.Maybe
import Data.Tuple

import Test.Unit
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.CouchDB
import Control.Coroutine.CouchDB

--newtype QCChange       = QCChange Change
--newtype QCResult       = QCResult Result
--newtype QCNotification = QCNotification Notification
--
--runQCChange :: QCChange -> Change
--runQCChange (QCChange change) = change
--
--runQCResult :: QCResult -> Result
--runQCResult (QCResult result) = result
--
--runQCNotification :: QCNotification -> Notification
--runQCNotification (QCNotification notification) = notification

popSTArray :: forall a h eff. STArray h a -> Eff (st :: ST h | eff) (Maybe a)
popSTArray stArr = do
  arr <- freeze stArr
  spliceSTArray stArr 0 1 []
  return $ head arr

getStubGet :: forall a b eff1 eff2. (Show a) =>
  Array (Either String a) -> Eff (st :: ST b | eff1) (Int -> Eff (st :: ST b | eff2) (Either String a))
getStubGet results = do
  mutableResults <- thaw results
  return $ \_ -> do
    currentResult <- popSTArray mutableResults
    return $ maybe (Left "Ran out of values") id currentResult

collect :: forall a m r. (Monad m) => Transformer a (Array a) m r
collect = tailRecM go []
  where
  go :: Array a -> Transformer a (Array a) m (Either (Array a) r)
  go xs = liftFreeT $ Transform \x -> Tuple (xs <> [x]) (Left (xs <> [x]))

main = do
  randomSample' 25 arbitrary >>= (flip foreachE) \notifications -> runTest do
    test "produceNotifications" do
      assertFn "should produce notifications sequentially" \done -> do
        stubGet <- getStubGet notifications
        let f xs = if length xs == length notifications then done (xs == notifications) else return unit
        let c = consumer (($> Nothing) <<< liftEff <<< f <<< (Right <$>))
        let p = produceNotifications (liftEff <<< stubGet) 0
        launchAff $ later $ runProcess (p $~ collect $$ c)
