module Test.Coroutine where

import Control.Coroutine (Consumer, Transformer, Transform(Transform), consumer, (~~), transform)
import Control.Monad.Aff (Aff())
import Control.Monad.Free.Trans (liftFreeT)
import Control.Monad.Rec.Class (class MonadRec, forever, tailRecM)
import Data.Array (length, take) as A
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Generic (class Generic, gEq)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Monad, Unit, unit, return, (<<<), (>>=), eq, (<>), ($), (==))

take :: forall a b m. (Monad m) => Int -> Transformer (Array a) (Maybe (Array a)) m b
take n = forever (transform maybeTake)
  where
  maybeTake xs | A.length xs == n = Just $ A.take n xs
  maybeTake _ = Nothing

collect :: forall a m r. (Monad m) => Transformer a (Array a) m r
collect = tailRecM go []
  where
  go xs = liftFreeT $ Transform \x -> Tuple (xs <> [x]) (Left (xs <> [x]))

checkEq :: forall a b m. (Eq a, MonadRec m) => Array a -> Transformer a (Maybe Boolean) m b
checkEq xs = check (A.length xs) (eq xs)

checkGEq :: forall a b m. (Generic a, MonadRec m) => Array a -> Transformer a (Maybe Boolean) m b
checkGEq xs = check (A.length xs) (gEq xs)

check :: forall a b m. (MonadRec m) => Int -> (Array a -> Boolean) -> Transformer a (Maybe Boolean) m b
check n p = collect ~~ (take n) ~~ assertPredicate
  where
  assertPredicate = forever (transform (>>= (return <<< p)))

done :: forall a b eff. (a -> Aff eff Unit) -> Consumer (Maybe a) (Aff eff) b
done f = consumer (($> Nothing) <<< maybe (return unit) f)
