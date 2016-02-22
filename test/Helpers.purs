module Test.Helpers where

import Prelude ((++), ($), (>>=))

import Data.Either (Either(), either)
import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Exception (Error, error)
import Data.Maybe (Maybe(..))
import Data.Array (findIndex, index)
import Data.Generic (class Generic, gShow)

stubGet :: forall a b eff. (Generic b) =>
  Array (Either Error a) -> (b -> a -> Boolean) -> b -> Aff eff a
stubGet results f x = makeAff \fail success -> case found of
  Just result -> either fail success result
  Nothing -> fail $ error $ "Test.Helpers.stubGet: Couldn't get \"" ++ gShow x ++ "\"."
    where
    found :: Maybe (Either Error a)
    found = findIndex (either (\_ -> false) (f x)) results >>= index results
