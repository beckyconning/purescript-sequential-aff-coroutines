module Test.Coroutine where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Free.Trans (liftFreeT)
import Control.Monad.Rec.Class (MonadRec, forever, tailRecM)
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Generic (Generic, gEq)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))

import qualified Data.Array (length, take) as A

import Control.Coroutine

take :: forall a b m. (Monad m) => Int -> Transformer (Array a) (Maybe (Array a)) m b
take n = forever (transform maybeTake)
  where
  maybeTake :: forall a. Array a -> Maybe (Array a)
  maybeTake xs | A.length xs == n = Just $ A.take n xs
  maybeTake _ = Nothing

collect :: forall a m r. (Monad m) => Transformer a (Array a) m r
collect = tailRecM go []
  where
  go :: Array a -> Transformer a (Array a) m (Either (Array a) r)
  go xs = liftFreeT $ Transform \x -> Tuple (xs <> [x]) (Left (xs <> [x]))

check :: forall a b c m. (MonadRec m) => Int -> (Array a -> b) -> Transformer a (Maybe b) m c
check n p = collect ~~ (take n) ~~ assertPredicate
  where
  assertPredicate = forever (transform (>>= (return <<< p)))

maybeCallback :: forall a b eff. (a -> Aff eff Unit) -> Consumer (Maybe a) (Aff eff) b
maybeCallback f = consumer (($> Nothing) <<< maybe (return unit) f)
