module Control.Coroutine.Aff.Seq where

import Control.Apply ((*>))
import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff, runAff, later)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..))
import Data.Functor (($>))
import Prelude

type AffGetter a b eff = b -> Aff (avar :: AVAR | eff) a
type SeqProducer a eff = Producer a (Aff (avar :: AVAR | eff)) Error

produceSeq :: forall a b eff. AffGetter a b eff -> (a -> b) -> b -> SeqProducer a eff
produceSeq get pluckSeq initialSeq = produce \emit -> void $ run emit $ getNext emit initialSeq
  where
  getNext emit seq = later $ get seq >>= emitAndGetNext emit
  emitAndGetNext emit x = (liftEff $ emit $ Left x) *> getNext emit (pluckSeq x)
  emitAndEnd emit error = liftEff $ emit $ Right error
  run emit = runAff (emitAndEnd emit) (const (pure unit))
