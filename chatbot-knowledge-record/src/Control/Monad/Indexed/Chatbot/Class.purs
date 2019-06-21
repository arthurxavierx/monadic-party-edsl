module Control.Monad.Indexed.Chatbot.Class where

import Prelude

import Control.Monad.Indexed (class IxMonad)
import Effect (Effect)
import Prim.Row (class Nub, class Union)
import Record (merge)
import Text.Parsing.Parser (Parser)

-- These types describe the basic protocol that chatbots follow:
--
--   Start -> Answer <-> Ask -> Finalized
--
data Start = Start
data Ask s = Ask {| s }
data Answer s = Answer {| s }
data Finalized = Finalized

-- | `Knows` describes types from which knowledge can be extracted in the form
-- | of a record.
class Knows (f :: # Type -> Type) where
  -- | Extracts the knowledge contained in `f`.
  _knows :: forall s. f s -> {| s }
  -- | Extends the knowledge in `f`.
  _learn :: forall r s t u. Union r s t => Nub t u => {| r } -> f s -> f u

instance knowsAsk :: Knows Ask where
  _knows (Ask s) = s
  _learn r (Ask s) = Ask (merge r s)

instance knowsAnswer :: Knows Answer where
  _knows (Answer s) = s
  _learn r (Answer s) = Answer (merge r s)

-- | `IxMonadChatbot` describes an indexed monad that supports chatbot
-- | operations.
class IxMonad m <= IxMonadChatbot m where
  -- | Initializes the chatbot by reading a message that can be parsed by the
  -- | specified parser. The record produced by the parser is added to the
  -- | knowledge record of the chatbot.
  start :: forall r. Parser String {| r } -> m Start (Answer r) Unit
  -- | Answers the user with a message.
  answer :: forall s. String -> m (Answer s) (Ask s) Unit
  -- | Asks the user a question and awaits for an answer that can be parsed by
  -- | the specified parser, producing a record that is added to the knowledge
  -- | repository of the chatbot.
  ask :: forall r s t u. Union r s t => Nub t u => String -> Parser String {| r } -> m (Ask s) (Answer u) Unit
  -- | Finalizes a chatbot, disabling its communication capabilities. After
  -- | this, an `IxMonadChatbot` essentially stops.
  finalize :: forall s. m (Ask s) Finalized Unit

  -- | Extracts the current knowledge of the chatbot without altering its state.
  knowledge :: forall f s. Knows f => m (f s) (f s) {| s }
  -- | Extends the current knowledge of the chatbot without altering its state.
  learn :: forall f r s t u. Knows f => Union r s t => Nub t u => {| r } -> m (f s) (f u) Unit

  -- | `effect` is used instead of `liftEffect` in order to reset the chatbot's
  -- | state to `Answer` after performing the effect.
  effect
    :: forall f s a
     . Knows f
    => ({| s } -> Effect a)
    -> m (f s) (Answer s) a
