module Main where

import Prelude

import Control.Alternative ((<|>))
import Control.Applicative.Indexed (class IxApplicative)
import Control.Monad.Indexed (ipure)
import Control.Monad.Indexed.Chatbot (ConsoleChatbot, Finalized, Start, answer, ask, finalize, runConsoleChatbot, start)
import Control.Monad.Indexed.Chatbot.Class (effect)
import Control.Monad.Indexed.Do as Ix
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Array (many)
import Data.Char.Unicode (isAlpha)
import Data.Char.Unicode as Unicode
import Data.Foldable (oneOf)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Effect (Effect)
import Effect.Aff (launchAff_)
import Text.Parsing.Parser (ParserT, fail)
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.String (anyChar, char)

main :: Effect Unit
main = launchAff_ $ runConsoleChatbot productOrdering

-- Preferred language chatbot {{{

data Language = Haskell | PureScript
derive instance eqLanguage :: Eq Language

preferredLanguage :: ConsoleChatbot Start Finalized Unit
preferredLanguage = Ix.do
  start any
  answer "Hey there!"
  language <- askPreferences
  loop assertPreferences language
  answer "Nice!"
  finalize
  where
    -- askPreferences :: ConsoleChatbot Ask Answer Language
    askPreferences = Ix.do
      ask "Tell me: which one do you prefer, Haskell or PureScript?" do
        oneOf
          [ Haskell <$ match "haskell"
          , PureScript <$ match "purescript"
          ]

    -- assertPreferences :: Language -> ConsoleChatbot Answer Answer (Step Language Unit)
    assertPreferences language = Ix.do
      if language == PureScript then Ix.do
        break unit
      else Ix.do
        answer "Errrr... Tell me again: which one do you prefer, Haskell or PureScript?"
        language' <- askPreferences
        continue language'

-- }}}

-- Product ordering chatbot {{{

type User =
  { name :: String
  , email :: String
  , country :: String
  }

type Order =
  { product :: String
  , quantity :: Int
  }

productOrdering :: ConsoleChatbot Start Finalized Unit
productOrdering = Ix.do
  start any

  answer "Hello, dear customer!\n"
  email <- ask "What is your email address?" anyString

  answer "\nHmm, let me see if I can find your records..."
  records <- effect $ findRecords email
  { name, country } <-
    case records of
      Nothing ->
        registerUser email
      Just r ->
        pure r

  answer $ "\nAlright! All set! Now let's order your product.\n"
  { product, quantity } <- orderProduct

  answer
    $ "\nAlright, " <> name <> ", it's done!\n"
    <> "We will ship your " <> show quantity <> " units of " <> product <> " to " <> country <> " soon.\n"
    <> "Stay tuned at " <> email <> " for updates!"

  finalize

  where
    -- registerUser :: String -> ConsoleChatbot Answer Answer User
    registerUser email = Ix.do
      answer
        $ "Oops, I couldn't find anything.\n"
        <> "Maybe you should get registered?\n\n"
      name <- ask "What is your name?" anyString
      answer "\n"
      country <- ask "And what country are you from?" anyString
      pure { email, name, country }

    -- orderProduct :: ConsoleChatbot Ask Answer Order
    orderProduct = Ix.do
      product <- ask  "What would you like to order?" anyString
      answer ""
      quantity <- ask ("And how many units of " <> product <> " do you want?") do
        s <- anyString
        case Int.fromString s of
          Nothing ->
            fail "Not a valid number."
          Just quantity ->
            pure quantity
      pure { product, quantity }

-- Users database {{{

user_testy :: User
user_testy =
  { name: "Testy McTestface"
  , email: "testy@test.com"
  , country: "Brazil"
  }

user_nobody :: User
user_nobody =
  { name: "Nobody Noone"
  , email: "nobody@gmail.com"
  , country: "United States of America"
  }

findRecords :: String -> Effect (Maybe User)
findRecords email
  | email == user_testy.email = pure $ Just user_testy
  | email == user_nobody.email = pure $ Just user_nobody
  | otherwise = pure Nothing

-- }}}

-- }}}

-- Utils {{{

-- | Alias for `tailRecM`.
loop :: forall m a b. MonadRec m => (b -> m (Step b a)) -> b -> m a
loop = tailRecM

-- | Syntactic sugar for continuing a `loop` with a value.
continue :: forall m x a b. IxApplicative m => b -> m x x (Step b a)
continue = ipure <<< Loop

-- | Syntactic sugar for breaking out of a `loop` with a value.
break :: forall m x a b. IxApplicative m => a -> m x x (Step b a)
break = ipure <<< Done

-- Parsing

-- | A `Parser` that matches anything, returning an empty record.
any :: forall s m. Monad m => ParserT s m Unit
any = pure unit

-- | A `Parser` that matches any string.
anyString :: forall m. Monad m => ParserT String m String
anyString = SCU.fromCharArray <$> many anyChar

-- | A `Parser` that matches the specified string with no case sensitivity.
match :: forall m. Monad m => String -> ParserT String m Unit
match name =
  case SCU.uncons name of
    Nothing ->
      pure unit
    Just { head: c, tail: cs } ->
      (caseChar c <?> msg) *> match cs
  where
    caseChar :: Char -> ParserT String m Char
    caseChar c
      | isAlpha c = char (Unicode.toLower c) <|> char (Unicode.toUpper c)
      | otherwise = char c

    msg :: String
    msg = show name

-- }}}
