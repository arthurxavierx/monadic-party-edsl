module Control.Monad.Indexed.Chatbot.Class where

import Prelude

import Control.Monad.Indexed (class IxMonad)
import Effect (Effect)
import Text.Parsing.Parser (Parser)

-- These types describe the basic protocol that chatbots follow:
--
--   Start -> Answer <-> Ask -> Finalized
--
data Start = Start
data Ask = Ask
data Answer = Answer
data Finalized = Finalized

-- | `IxMonadChatbot` describes an indexed monad that supports chatbot
-- | operations.
class IxMonad m <= IxMonadChatbot m where
  -- | Initializes the chatbot by reading a message that can be parsed by the
  -- | specified parser. The record produced by the parser is added to the
  -- | knowledge record of the chatbot.
  start :: forall a. Parser String a -> m Start Answer a
  -- | Asks the user a question and awaits for an answer that can be parsed by
  -- | the specified parser, producing a record that is added to the knowledge
  -- | repository of the chatbot.
  ask :: forall a. String -> Parser String a -> m Ask Answer a
  -- | Answers the user with a message.
  answer :: String -> m Answer Ask Unit
  -- | Finalizes a chatbot, disabling its communication capabilities. After
  -- | this, an `IxMonadChatbot` essentially stops.
  finalize :: m Ask Finalized Unit

  -- | `effect` is used instead of `liftEffect` in order to reset the chatbot's
  -- | state to `Answer` after performing the effect.
  effect :: forall i a. Effect a -> m i Answer a
