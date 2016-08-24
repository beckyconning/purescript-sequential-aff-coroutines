module Test.Helpers where

import Prelude

import Data.Either (Either(..), either)
import Control.Monad.Aff (Aff(), makeAff, launchAff)
import Control.Monad.Eff.Exception (error, Error())
import Data.Maybe (Maybe(..))
import Data.Array (findIndex, index)
import Data.Generic (Generic, gShow)

stubGet :: forall a b eff. (Generic b) => (b -> a -> Boolean) -> Array a -> b -> Aff eff a
stubGet f results x = makeAff \fail success -> case maybeResult of
  Just result -> success result
  Nothing -> fail $ error $ "Test.Helpers.stubGet: Couldn't get \"" ++ gShow x ++ "\"."
    where
    maybeResult :: Maybe a
    maybeResult = findIndex (f x) results >>= index results
