module Control.Monad.Indexed.State.Class where

import Prelude

import Control.Monad.Indexed (class IxMonad)
import Data.Tuple (Tuple(..))

class IxMonad m <= IxMonadState m where
  istate :: forall x y a. (x -> Tuple y a) -> m x y a

iget :: forall m x. IxMonadState m => m x x x
iget = istate \s -> Tuple s s

igets :: forall m x a. IxMonadState m => (x -> a) -> m x x a
igets f = istate \x -> Tuple x (f x)

iput :: forall m x y. IxMonadState m => x -> m y x Unit
iput s = istate \_ -> Tuple s unit

imodify :: forall m x y. IxMonadState m => (x -> y) -> m x y y
imodify f = istate \x -> let y = f x in Tuple y y

imodify_ :: forall m x y. IxMonadState m => (x -> y) -> m x y Unit
imodify_ f = istate \s -> Tuple (f s) unit
