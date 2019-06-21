{-# LANGUAGE DuplicateRecordFields #-}

module Types where

import Prelude

type Cents = Integer
type Country = String
type EmailAddress = String
type Password = String

data Registration = Registration
  { registrationFirstName :: String
  , registrationLastName :: String
  , registrationEmail :: EmailAddress
  , registrationPassword :: Password
  , registrationCountry :: Maybe Country
  }

data User = User
  { firstName :: String
  , lastName :: String
  , email :: EmailAddress
  , country :: Maybe Country
  , billingInfo :: Maybe BillingInfo
  }

data BillingInfo = BillingInfo
  { cardNumber :: String
  , cardName :: String
  }

data Purchase = Purchase
  { orderedProduct :: Product
  , orderedQuantity :: Int
  }

data Product = Product
  { name :: String
  , quantity :: Int
  , price :: Maybe Cents
  , availability :: [Country]
  }
