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

take :: forall a m. (Monad m) => Int -> Transformer (Array a) (Maybe (Array a)) m Unit
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

checkEq :: forall a m. (Eq a, MonadRec m) => Array a -> Transformer a (Maybe Boolean) m Unit
checkEq xs = check (A.length xs) (eq xs)

checkGEq :: forall a m. (Generic a, MonadRec m) => Array a -> Transformer a (Maybe Boolean) m Unit
checkGEq xs = check (A.length xs) (gEq xs)

check :: forall a m. (MonadRec m) => Int -> (Array a -> Boolean) -> Transformer a (Maybe Boolean) m Unit
check n p = collect ~~ (take n) ~~ assertPredicate
  where
  assertPredicate = forever (transform (>>= (return <<< p)))

done :: forall a eff. (a -> Aff eff Unit) -> Consumer (Maybe a) (Aff eff) Unit
done f = consumer (($> Nothing) <<< maybe (return unit) f)
