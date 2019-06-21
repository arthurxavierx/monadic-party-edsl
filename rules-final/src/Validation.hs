{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Validation where

import Prelude

import Control.Applicative (Alternative)
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(..))

newtype Validation env a = Validation (env -> Maybe a)

-- Instances {{{

instance Functor (Validation env) where
  fmap f (Validation k) = Validation $ \env -> fmap f (k env)

instance Applicative (Validation env) where
  Validation kf <*> Validation ka =
    Validation $ \env ->
      kf env <*> ka env

  pure a = Validation $ \_ -> pure a

instance Monad (Validation env) where
  Validation k >>= f =
    Validation $ \env -> do
      a <- k env
      let Validation k' = f a
      k' env

  return = pure

-- }}}

-- | Runs a validation rule given an environment.
runValidation :: Validation env a -> env -> Maybe a
runValidation (Validation r) = r

-- | A validation rule that fails if the given boolean value is false.
is :: Bool -> Validation env ()
is True = Validation $ \_ -> Just ()
is False = Validation $ \_ -> Nothing

-- | A validation rule that checks whether a Maybe is a Just, returning the
-- wrapped value if so.
exists :: Maybe a -> Validation env a
exists = Validation . const

-- | A validation rule that always passes and returns the current environment.
getEnv :: Validation env env
getEnv = Validation $ \env -> pure env

-- Syntax sugar {{{

-- | Syntax sugar for accessing fields of domain entities.
infixr 9 `of_`
of_ :: (a -> b) -> a -> b
of_ = ($)

-- | Syntax sugar for finalizing a validation rule with an empty value.
done :: Monad m => m ()
done = return ()

-- }}}
