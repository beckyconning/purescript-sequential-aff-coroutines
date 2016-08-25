module Control.Coroutine.Aff.Seq where

import Control.Apply ((*>))
import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produceAff)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Free.Trans (hoistFreeT)
import Control.Monad.Aff.Free (class Affable, fromAff)
import Data.Either (Either(..))
import Data.Functor (($>))
import Prelude

type AffGetter a b eff = b -> Aff (avar :: AVAR | eff) a

produceSeq :: forall a b m eff. (Affable (avar :: AVAR | eff) m, Functor m) => AffGetter a b eff -> (a -> b) -> b -> Producer a m Unit
produceSeq get pluckSeq initialSeq = produceAff' \emit -> getNext emit initialSeq
  where
  getNext emit seq = get seq >>= emitAndGetNext emit
  emitAndGetNext emit x = (emit $ Left x) *> getNext emit (pluckSeq x)

produceAff'
  :: forall a r m eff
   . (Affable (avar :: AVAR | eff) m, Functor m)
  => ((Either a r -> Aff (avar :: AVAR | eff) Unit) -> Aff (avar :: AVAR | eff) Unit)
  -> Producer a m r
produceAff' = hoistFreeT fromAff <<< produceAff
