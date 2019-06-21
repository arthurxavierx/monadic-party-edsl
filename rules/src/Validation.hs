{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Validation where

import Prelude

import Control.Applicative (Alternative)
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(..))

newtype Validation env a = Validation _

-- Instances {{{

instance Functor (Validation env) where
  fmap = _

instance Applicative (Validation env) where
  (<*>) = _
  pure = _

instance Monad (Validation env) where
  (>>=) = _
  return = _

-- }}}

-- | Runs a validation rule given an environment.
runValidation :: Validation env a -> env -> Maybe a
runValidation = _

-- | A validation rule that fails if the given boolean value is false.
is :: Bool -> Validation env ()
is = _

-- | A validation rule that checks whether a Maybe is a Just, returning the
-- wrapped value if so.
exists :: Maybe a -> Validation env a
exists = _

-- | A validation rule that always passes and returns the current environment.
getEnv :: Validation env env
getEnv = _

-- Syntax sugar {{{

-- | Syntax sugar for accessing fields of domain entities.
infixr 9 `of_`
of_ :: (a -> b) -> a -> b
of_ = ($)

-- | Syntax sugar for finalizing a validation rule with an empty value.
done :: Monad m => m ()
done = return ()

-- }}}
