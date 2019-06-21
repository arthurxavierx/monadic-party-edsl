module Control.Monad.Indexed.Chatbot
  ( ConsoleChatbot
  , runConsoleChatbot
  , module Control.Monad.Indexed.Chatbot.Class
  ) where

import Prelude

import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad)
import Control.Monad.Indexed.Chatbot.Class (class IxMonadChatbot, Answer(..), Ask(..), Finalized(..), Start(..))
import Control.Monad.Indexed.Chatbot.Class (class IxMonadChatbot, Answer, Ask, Finalized, Start, answer, ask, finalize, start) as Control.Monad.Indexed.Chatbot.Class
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Trans as ReaderT
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.Console (Interface, closeInterface, createInterface, prompt)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)

newtype ConsoleChatbot x y a = ConsoleChatbot (ReaderT Interface Aff a)

runConsoleChatbot :: forall a. ConsoleChatbot Start Finalized a -> Aff a
runConsoleChatbot (ConsoleChatbot r) =
  bracket
    (liftEffect createInterface)
    (liftEffect <<< closeInterface)
    (runReaderT r)

derive instance newtypeConsoleChatbot :: Newtype (ConsoleChatbot x y a) _
derive newtype instance functorConsoleChatbot :: Functor (ConsoleChatbot x y)
derive newtype instance applyConsoleChatbot :: Apply (ConsoleChatbot x x)
derive newtype instance applicativeConsoleChatbot :: Applicative (ConsoleChatbot x x)
derive newtype instance bindConsoleChatbot :: Bind (ConsoleChatbot x x)
derive newtype instance monadConsoleChatbot :: Monad (ConsoleChatbot x x)
derive newtype instance monadRecConsoleChatbot :: MonadRec (ConsoleChatbot x x)

instance ixFunctorConsoleChatbot :: IxFunctor ConsoleChatbot where
  imap f (ConsoleChatbot r) = ConsoleChatbot (map f r)

instance ixApplyConsoleChatbot :: IxApply ConsoleChatbot where
  iapply (ConsoleChatbot f) (ConsoleChatbot a) = ConsoleChatbot (f <*> a)

instance ixApplicativeConsoleChatbot :: IxApplicative ConsoleChatbot where
  ipure a = ConsoleChatbot (pure a)

instance ixBindConsoleChatbot :: IxBind ConsoleChatbot where
  ibind (ConsoleChatbot r) f =
    ConsoleChatbot $ bind r \a ->
      let
        ConsoleChatbot r' = f a
      in
        r'

instance ixMonadConsoleChatbot :: IxMonad ConsoleChatbot

instance ixMonadChatbotConsoleChatbot :: IxMonadChatbot ConsoleChatbot where
  start :: forall a. Parser String a -> ConsoleChatbot Start Answer a
  start parser = ConsoleChatbot do
    Tuple _ r <- unit # tailRecM \_ -> do
      -- Prompts for a message and loops until it can be parsed.
      msg <- prompt =<< ReaderT.ask
      case runParser msg parser of
        Left err -> do
          log $ "Invalid message: " <> parseErrorMessage err
          pure $ Loop unit
        Right r ->
          pure $ Done (Tuple msg r)
    pure r

  ask :: forall a. String -> Parser String a -> ConsoleChatbot Ask Answer a
  ask q parser = ConsoleChatbot do
    Tuple _ r <- unit # tailRecM \_ -> do
      -- Ask the question
      liftEffect $ log q
      -- Prompts for a message and loops until it can be parsed.
      msg <- prompt =<< ReaderT.ask
      case runParser msg parser of
        Left err -> do
          log $ "Invalid message: " <> parseErrorMessage err
          pure $ Loop unit
        Right r ->
          pure $ Done (Tuple msg r)
    pure r

  answer :: String -> ConsoleChatbot Answer Ask Unit
  answer msg = ConsoleChatbot $ liftEffect $ log msg

  finalize :: ConsoleChatbot Ask Finalized Unit
  finalize = ConsoleChatbot (pure unit)

  effect :: forall i a. Effect a -> ConsoleChatbot i Answer a
  effect = ConsoleChatbot <<< liftEffect
