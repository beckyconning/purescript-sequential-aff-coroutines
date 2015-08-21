module Test.Helpers where

import Prelude

import Data.Either (Either(..), either)
import Control.Monad.Aff (Aff(), makeAff, launchAff)
import Control.Monad.Eff.Exception (error)
import Data.Maybe (Maybe(..))
import Data.Array (findIndex, index)
import Data.Generic (Generic, gShow)

stubGet :: forall a b eff. (Generic b) =>
  Array (Either Unit a) -> (b -> a -> Boolean) -> b -> Aff eff (Either Unit a)
stubGet results f x = makeAff \fail success -> case found of
  Just result -> success result
  Nothing -> fail $ error $ "Test.Helpers.stubGet: Couldn't get \"" ++ gShow x ++ "\"."
    where
    found :: Maybe (Either Unit a)
    found = findIndex (either (\_ -> false) (f x)) results >>= index results
