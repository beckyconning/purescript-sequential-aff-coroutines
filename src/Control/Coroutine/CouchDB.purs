module Control.Coroutine.CouchDB where

import Prelude

-- TODO: Make imports more specific
import Data.Maybe
import Data.Either (Either(..), either)
import Data.Functor (($>))
import Data.CouchDB (Notification(..))

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

type NotificationGetter e eff = Int -> Aff (avar :: AVAR | eff) (Either e Notification)
type NotificationProducer e eff = Producer Notification (Aff (avar :: AVAR | eff)) e

produceNotifications :: forall e eff. NotificationGetter e eff -> Int -> NotificationProducer e eff
produceNotifications get initialSeq = produce \emit -> launchAff $ getNext emit initialSeq
  where
  getNext emit seq = later $ get seq >>= either (emitAndEnd emit) (emitAndGetNext emit)
  emitAndGetNext emit n@(Notification o) = (liftEff $ emit $ Left n) *> getNext emit o.last_seq
  emitAndEnd emit error = liftEff $ emit $ Right error
