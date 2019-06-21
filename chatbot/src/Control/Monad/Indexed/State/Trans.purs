module Control.Monad.Indexed.State.Trans where

import Prelude

import Control.Applicative.Indexed (class IxApplicative)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Indexed (class IxMonad, iap)
import Control.Monad.Indexed.State.Class (class IxMonadState)
import Control.Monad.Indexed.Trans (class IxMonadTrans, ilift)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Functor.Indexed (class IxFunctor)
import Data.Newtype (class Newtype, over, un)
import Data.Tuple (Tuple(..), snd)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

newtype IxStateT m x y a = IxStateT (x -> m (Tuple y a))

runIxStateT :: forall m x y a. Functor m => IxStateT m x y a -> x -> m a
runIxStateT = map (map snd) <<< un IxStateT

execIxStateT :: forall m x y a. IxStateT m x y a -> x -> m (Tuple y a)
execIxStateT = un IxStateT

-- Instances {{{

derive instance newtypeIxStateT :: Newtype (IxStateT m x y a) _

derive instance functorIxState :: Functor m => Functor (IxStateT m x y)

instance applyIxStateT :: Monad m => Apply (IxStateT m x x) where
  apply = ap

instance applicativeIxStateT :: Monad m => Applicative (IxStateT m x x) where
  pure a = IxStateT \x -> pure (Tuple x a)

instance bindIxStateT :: Monad m => Bind (IxStateT m x x) where
  bind (IxStateT k) f =
    IxStateT \x -> do
      Tuple y a <- k x
      let IxStateT k' = f a
      k' y

instance monadIxStateT :: Monad m => Monad (IxStateT m x x)

instance monadRecIxStateT :: MonadRec m => MonadRec (IxStateT m x x) where
  tailRecM f a = IxStateT \s -> tailRecM f' (Tuple s a)
    where
      f' (Tuple s a') =
        case f a' of IxStateT st ->
          st s >>= \(Tuple s1 m) ->
            pure case m of
              Loop x -> Loop (Tuple s1 x)
              Done y -> Done (Tuple s1 y)

instance monadEffectIxStateT :: MonadEffect m => MonadEffect (IxStateT m x x) where
  liftEffect = ilift <<< liftEffect

instance monadAffIxStateT :: MonadAff m => MonadAff (IxStateT m x x) where
  liftAff = ilift <<< liftAff

-- }}}

-- IxMonad instances {{{

instance ixFunctorIxStateT :: Functor m => IxFunctor (IxStateT m) where
  imap = over IxStateT <<< map <<< map <<< map

instance ixApplyIxStateT :: Monad m => IxApply (IxStateT m) where
  iapply = iap

instance ixApplicativeIxStateT :: Monad m => IxApplicative (IxStateT m) where
  ipure a = IxStateT \x -> pure (Tuple x a)

instance ixBindIxStateT :: Monad m => IxBind (IxStateT m) where
  ibind (IxStateT k) f =
    IxStateT \x -> do
      Tuple y a <- k x
      let IxStateT k' = f a
      k' y

instance ixMonadIxStateT :: Monad m => IxMonad (IxStateT m)

instance ixMonadTransIxStateT :: IxMonadTrans IxStateT where
  ilift m = IxStateT \x -> Tuple x <$> m

-- }}}

instance ixMonadStateIxStateT :: Monad m => IxMonadState (IxStateT m) where
  istate f = IxStateT \x -> pure (f x)
