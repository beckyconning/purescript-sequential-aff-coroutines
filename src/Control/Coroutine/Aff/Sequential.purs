module Control.Coroutine.Aff.Seq where

import Prelude

-- TODO: Make imports more specific
import Data.Maybe
import Data.Either (Either(..), either)
import Data.Functor (($>))

import Control.Apply ((*>))
import Control.Coroutine
import Control.Coroutine.Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception hiding (error)
import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Control.Monad.Trans

type AffGetter a b err eff = b -> Aff (avar :: AVAR | eff) (Either err a)
type SeqProducer a err eff = Producer a (Aff (avar :: AVAR | eff)) err

produceSeq :: forall a b err eff. AffGetter a b err eff -> (a -> b) -> b -> SeqProducer a err eff
produceSeq get pluckSeq initialSeq = produce \emit -> launchAff $ getNext emit initialSeq
  where
  getNext emit seq = later $ get seq >>= either (emitAndEnd emit) (emitAndGetNext emit)
  emitAndGetNext emit x = (liftEff $ emit $ Left x) *> getNext emit (pluckSeq x)
  emitAndEnd emit error = liftEff $ emit $ Right error
