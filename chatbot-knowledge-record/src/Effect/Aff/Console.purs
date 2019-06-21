module Effect.Aff.Console where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)

foreign import data Interface :: Type

foreign import createInterface :: Effect Interface

foreign import closeInterface :: Interface -> Effect Unit

prompt :: forall m. MonadAff m => Interface -> m String
prompt interface = question interface ""

question :: forall m. MonadAff m => Interface -> String -> m String
question interface msg = do
  liftAff $ makeAff \cb -> do
    _question interface msg (cb <<< Right)
    pure nonCanceler

foreign import _question :: Interface -> String -> (String -> Effect Unit) -> Effect Unit
