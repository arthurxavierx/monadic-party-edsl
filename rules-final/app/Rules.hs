{-# LANGUAGE RebindableSyntax #-}

module Rules where

import Prelude hiding (product)
import Rule
import Types

import Data.Semigroup ((<>))

-- Event {{{

data Event
  = BuyProduct Purchase
  | Register Registration
  | ViewProduct Product

buyProduct :: Match Event Purchase
buyProduct (BuyProduct p) = Just p
buyProduct _ = Nothing

register :: Match Event Registration
register (Register r) = Just r
register _ = Nothing

viewProduct :: Match Event Product
viewProduct (ViewProduct p) = Just p
viewProduct _ = Nothing

-- }}}

-- Env {{{

data Env = Env
  { envUser :: Maybe User
  -- , ...
  }

-- }}}

rules :: Rules Event Env IO
rules = do
  to buyProduct validatePurchase
  to buyProduct validateUserBillingInfo
  after buyProduct sendPurchaseEmail
  where
    (>>) = (<>)

authenticate = do
  user <- envUser <$> getEnv
  exists user

available q product = quantity `of_` product >= q

validatePurchase purchase = do
  user <- authenticate
  is (available (orderedQuantity `of_` purchase) (orderedProduct `of_` purchase))
  exists (price `of_` (orderedProduct `of_` purchase))
  exists (country `of_` user)
  done

validateUserBillingInfo _ = do
  user <- authenticate
  exists (billingInfo `of_` user)

sendPurchaseEmail _ _ =
  putStrLn "Sending an email to the user"
