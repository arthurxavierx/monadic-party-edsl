{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rule
  ( module Validation
  , Match
  , Rules
  , to
  , after
  , dispatch
  ) where

import Prelude
import Validation

import Control.Monad (void)
import Data.Semigroup (Semigroup)

-- | Given a type `f`, `Match f a` is a function that tries to extract an `a`
-- out of any given `f`.  This is basically a *Prism*.
type Match f a = f -> Maybe a

-- Rules are simply a list of individual, independent rules.
newtype Rules event env m = Rules [Rule event env m]
  deriving (Semigroup, Monoid)

-- | Validate an event given a function that takes the contents of the event as
-- an argument and produces a validation rule.
to :: Match event e -> (e -> Validation env a) -> Rules event env m
to match = Rules . pure . Guard match

-- | Produce an effectful action after an event has been dispatched, taking in
-- an environment and the contents of the event as arguments.
after :: Match event e -> (env -> e -> m a) -> Rules event env m
after match = Rules . pure . After match

-- | A rule is either a guard-/to-rule that validates an event or an after-rule
-- that produces effectful actions after an event.
--
-- This type is not exported from this module.
data Rule event env m where
  Guard :: Match event e -> (e -> Validation env a) -> Rule event env m
  After :: Match event e -> (env -> e -> m a) -> Rule event env m

-- | Given a set of rules and an environment, dispatch a specific event,
-- collecting the results of the executed rules.
--
-- `Nothing` means that a validation has failed.
-- `Just _` means that no validation has failed — even though no rules exist
--          in the set for such event.
dispatch
  :: forall event e env m
   . Applicative m
  => Rules event env m
  -> env
  -> event
  -> Maybe (m ())
dispatch (Rules rules) env event =
  case rules of
    -- An empty rule set is always valid but does nothing.
    [] ->
      Just $ pure ()
    -- A rule set with a single rule is interpreted by running that single rule.
    [r] ->
      run r
    -- A rule set with multiple rules is interpreted by running all of them in
    -- sequence.
    (r:rs) ->
      (*>) <$> run r <*> dispatch (Rules rs) env event
  where
    run :: Rule event env m -> Maybe (m ())
    -- Running a `Guard` rule means extracting the event contents, passing them to
    -- the validation function, then running the result validation rule.
    --
    -- Since the output of `runValidation` is a `Maybe`, chaining multiple of
    -- those gives us the required short-circuiting behaviour.
    run (Guard match v) =
      case match event of
        Nothing ->
          Just $ pure ()
        Just e ->
          pure () <$ runValidation (v e) env
    -- Running a `After` rule means extracting the event contents, and passing
    -- them to the effectful function.
    run (After match action) =
      case match event of
        Nothing ->
          Just $ pure ()
        Just e ->
          Just $ void $ action env e
