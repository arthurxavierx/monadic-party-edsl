module Control.Monad.Indexed.Do where

import Control.Applicative.Indexed (class IxApplicative, ipure)
import Control.Apply.Indexed (class IxApply, iapply)
import Control.Bind.Indexed (class IxBind, ibind)
import Data.Functor.Indexed (class IxFunctor, imap)
import Prelude (class Discard)

map :: forall a b x y f. IxFunctor f => (a -> b) -> f x y a -> f x y b
map = imap

apply :: forall a b x y z m. IxApply m => m x y (a -> b) -> m y z a -> m x z b
apply = iapply

pure :: forall a x m. IxApplicative m => a -> m x x a
pure = ipure

bind :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
bind = ibind

discard :: forall m a b x y z. Discard a => IxBind m => m x y a -> (a -> m y z b) -> m x z b
discard = ibind
