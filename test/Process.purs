module Test.Process where

import Prelude
import Control.Monad.Eff

foreign import data EXIT :: !

foreign import data COLORLOG :: !

foreign import exit :: forall eff. Int -> Eff (exit :: EXIT | eff) Unit

foreign import logRed :: forall eff. String -> Eff (colorLog :: COLORLOG | eff) Unit

foreign import logGreen :: forall eff. String -> Eff (colorLog :: COLORLOG | eff) Unit
