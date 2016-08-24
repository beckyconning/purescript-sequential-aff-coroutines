module Control.Coroutine.Aff.Seq where

import Control.Apply ((*>))
import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce)
import Control.Monad.Aff (Aff, runAff, later)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Either (Either(..), either)
import Data.Functor (($>))
import Prelude

type AffGetter a b err eff = b -> Aff (avar :: AVAR | eff) (Either err a)
type SeqProducer a err eff = Producer a (Aff (avar :: AVAR | eff)) (Either Error err)

-- Should the pluck return Either?

produceSeq :: forall a b err eff. AffGetter a b err eff -> (a -> b) -> b -> SeqProducer a err eff
produceSeq get pluckSeq initialSeq =
  produce \emit ->
    void $ runAff (emit <<< Right <<< Left) (const $ pure unit) $ getNext emit initialSeq
  where
  getNext emit seq = later $ get seq >>= either emitAndEnd emitAndGetNext
    where
    emitAndGetNext x = (liftEff $ emit $ Left x) *> getNext emit (pluckSeq x)
    emitAndEnd error = liftEff $ emit $ Right $ Right error
