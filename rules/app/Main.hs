{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Prelude
import Rule
import Rules
import Types

main :: IO ()
main =
  case dispatch rules env (BuyProduct purchase1) of
    Nothing ->
      putStrLn "Validation failed!"
    Just io ->
      io
  where
    env =
      Env
        { envUser = Just user_nobody
        }

-- Data fixtures {{{

user_testy :: User
user_testy = User
  { firstName = "Testy"
  , lastName = "McTestface"
  , email = "testy@test.com"
  , country = Just "Brazil"
  , billingInfo = Just $ BillingInfo
      { cardNumber = "TEST123"
      , cardName = "TESTY MCTESTFACE"
      }
  }

user_nobody :: User
user_nobody = User
  { firstName = "Nobody"
  , lastName = "Noone"
  , email = "nobody@gmail.com"
  , country = Just "United States of America"
  , billingInfo = Nothing
  }

product_nothing :: Product
product_nothing = Product
  { name = "Test product"
  , quantity = 1000
  , price = Just 999
  , availability = ["Brazil", "Poland"]
  }

purchase1 :: Purchase
purchase1 = Purchase
  { orderedProduct = product_nothing
  , orderedQuantity = 55
  }

-- }}}
