module Control.Monad.Indexed.Chatbot
  ( ConsoleChatbot
  , runConsoleChatbot
  , module Control.Monad.Indexed.Chatbot.Class
  ) where

import Prelude

import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad, ipure)
import Control.Monad.Indexed.Chatbot.Class (class IxMonadChatbot, class Knows, Answer(..), Ask(..), Finalized(..), Start(..), _knows, _learn, knowledge)
import Control.Monad.Indexed.Chatbot.Class (class IxMonadChatbot, Answer, Ask, Finalized, Start, answer, ask, effect, finalize, knowledge, learn, start) as Control.Monad.Indexed.Chatbot.Class
import Control.Monad.Indexed.Do as Ix
import Control.Monad.Indexed.State.Class (iget, imodify_, iput)
import Control.Monad.Indexed.State.Trans (IxStateT, runIxStateT)
import Control.Monad.Indexed.Trans (ilift)
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
import Prim.Row (class Nub, class Union)
import Record (merge)
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)

newtype ConsoleChatbot x y a = ConsoleChatbot (IxStateT (ReaderT Interface Aff) x y a)

runConsoleChatbot :: forall a. ConsoleChatbot Start Finalized a -> Aff a
runConsoleChatbot (ConsoleChatbot i) =
  bracket
    (liftEffect createInterface)
    (liftEffect <<< closeInterface)
    (runReaderT (runIxStateT i Start))

derive instance newtypeConsoleChatbot :: Newtype (ConsoleChatbot x y a) _
derive newtype instance functorConsoleChatbot :: Functor (ConsoleChatbot x y)
derive newtype instance applyConsoleChatbot :: Apply (ConsoleChatbot x x)
derive newtype instance applicativeConsoleChatbot :: Applicative (ConsoleChatbot x x)
derive newtype instance bindConsoleChatbot :: Bind (ConsoleChatbot x x)
derive newtype instance monadConsoleChatbot :: Monad (ConsoleChatbot x x)
derive newtype instance monadRecConsoleChatbot :: MonadRec (ConsoleChatbot x x)
derive newtype instance ixFunctorConsoleChatbot :: IxFunctor ConsoleChatbot
derive newtype instance ixApplyConsoleChatbot :: IxApply ConsoleChatbot
derive newtype instance ixApplicativeConsoleChatbot :: IxApplicative ConsoleChatbot
derive newtype instance ixBindConsoleChatbot :: IxBind ConsoleChatbot
derive newtype instance ixMonadConsoleChatbot :: IxMonad ConsoleChatbot

instance ixMonadChatbotConsoleChatbot :: IxMonadChatbot ConsoleChatbot where
  start :: forall r. Parser String {| r } -> ConsoleChatbot Start (Answer r) Unit
  start parser = ConsoleChatbot Ix.do
    Tuple msg r <- ilift $ unit # tailRecM \_ -> do
      -- Prompts for a message and loops until it can be parsed.
      msg <- prompt =<< ReaderT.ask
      case runParser msg parser of
        Left err -> do
          log $ "Invalid message: " <> parseErrorMessage err
          pure $ Loop unit
        Right r ->
          pure $ Done (Tuple msg r)
    imodify_ \_ -> Answer r

  ask :: forall r s t u. Union r s t => Nub t u => String -> Parser String {| r } -> ConsoleChatbot (Ask s) (Answer u) Unit
  ask q parser = ConsoleChatbot Ix.do
    Tuple msg r <- ilift $ unit # tailRecM \_ -> do
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
    imodify_ \(Ask s) -> Answer (merge r s)

  answer :: forall s. String -> ConsoleChatbot (Answer s) (Ask s) Unit
  answer msg = ConsoleChatbot Ix.do
    ilift $ liftEffect $ log msg
    imodify_ (Ask <<< _knows)

  effect :: forall f s a. Knows f => ({| s } -> Effect a) -> ConsoleChatbot (f s) (Answer s) a
  effect eff = Ix.do
    s <- knowledge
    ConsoleChatbot Ix.do
      a <- ilift $ liftEffect (eff s)
      imodify_ (Answer <<< _knows)
      ipure a

  knowledge :: forall f s. Knows f => ConsoleChatbot (f s) (f s) {| s }
  knowledge = ConsoleChatbot (map _knows iget)

  learn :: forall f r s t u. Knows f => Union r s t => Nub t u => {| r } -> ConsoleChatbot (f s) (f u) Unit
  learn = ConsoleChatbot <<< imodify_ <<< _learn

  finalize :: forall s. ConsoleChatbot (Ask s) Finalized Unit
  finalize = ConsoleChatbot (iput Finalized)
