module Main where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Indexed (class IxApplicative, ipure)
import Control.Monad.Indexed.Chatbot (Answer, Ask, ConsoleChatbot, Finalized, Start, answer, ask, effect, finalize, knowledge, learn, runConsoleChatbot, start)
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
import Prim.Row (class Nub)
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
  askPreferences
  loop assertPreferences
  answer "Nice!"
  finalize
  where
    askPreferences :: forall k₀ k. Nub (prefers :: Language | k₀) k => ConsoleChatbot (Ask k₀) (Answer k) Unit
    askPreferences = Ix.do
      ask "Tell me: which one do you prefer, Haskell or PureScript?" do
        prefers <- oneOf
          [ Haskell <$ match "haskell"
          , PureScript <$ match "purescript"
          ]
        pure { prefers }

    assertPreferences = Ix.do
      { prefers } <- knowledge
      if prefers == PureScript then Ix.do
        break unit
      else Ix.do
        answer "Errrr... Tell me again: which one do you prefer, Haskell or PureScript?"
        askPreferences
        continue

-- }}}

-- Product ordering chatbot {{{

type Product r = (product :: String, quantity :: Int | r)
type User r = (name :: String, email :: String, country :: String | r)

productOrdering :: ConsoleChatbot Start Finalized Unit
productOrdering = Ix.do
  start any

  answer "Hello, dear customer!\n"
  ask "What is your email address?" do
    { email: _ } <$> anyString

  answer "\nHmm, let me see if I can find your records..."
  records <- effect findRecords
  case records of
    Nothing ->
      registerUser
    Just r ->
      learn r

  answer $ "\nAlright! All set! Now let's order your product.\n"
  orderProduct

  { email, name, country, product, quantity } <- knowledge
  answer
    $ "\nAlright, " <> name <> ", it's done!\n"
    <> "We will ship your " <> show quantity <> " units of " <> product <> " to " <> country <> " soon.\n"
    <> "Stay tuned at " <> email <> " for updates!"

  finalize

  where
    -- registerUser :: forall k. ConsoleChatbot (Answer (email :: String | k)) (Answer (User k)) Unit
    registerUser = Ix.do
      answer
        $ "Oops, I couldn't find anything.\n"
        <> "Maybe you should get registered?\n\n"
      ask "What is your name?" $ { name: _ } <$> anyString
      answer "\n"
      ask "And what country are you from?" $ { country: _ } <$> anyString

    -- orderProduct :: forall k. ConsoleChatbot (Ask (User k)) (Answer (Product + User + k)) Unit
    orderProduct = Ix.do
      ask  "What would you like to order?" do
        { product: _ } <$> anyString
      { product } <- knowledge
      answer ""
      ask ("And how many units of " <> product <> " do you want?") do
        s <- anyString
        case Int.fromString s of
          Nothing ->
            fail "Not a valid number."
          Just quantity ->
            pure { quantity }

-- Users database {{{

user_testy :: Record (User ())
user_testy =
  { name: "Testy McTestface"
  , email: "testy@test.com"
  , country: "Brazil"
  }

user_nobody :: Record (User ())
user_nobody =
  { name: "Nobody Noone"
  , email: "nobody@gmail.com"
  , country: "United States of America"
  }

findRecords :: forall k. { email :: String | k } -> Effect (Maybe {| User () })
findRecords { email }
  | email == user_testy.email = pure $ Just user_testy
  | email == user_nobody.email = pure $ Just user_nobody
  | otherwise = pure Nothing

-- }}}

-- }}}

-- Utils {{{

-- | Loops a `MonadRec` until a `Just` value is produced.
loop :: forall m a. MonadRec m => m (Maybe a) -> m a
loop m = unit # tailRecM \_ ->
  m >>= case _ of
    Nothing ->
      pure $ Loop unit
    Just a ->
      pure $ Done a

-- | Syntactic sugar for continuing a `loop`.
continue :: forall m x a. IxApplicative m => m x x (Maybe a)
continue = ipure Nothing

-- | Syntactic sugar for breaking out of a `loop` with a value.
break :: forall m x a. IxApplicative m => a -> m x x (Maybe a)
break = ipure <<< Just

-- Parsing

-- | A `Parser` that matches anything, returning an empty record.
any :: forall s m. Monad m => ParserT s m {}
any = pure {}

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
