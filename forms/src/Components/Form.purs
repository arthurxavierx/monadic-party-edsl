module Components.Form where

import Prelude

import Data.Either (Either(..))
import Data.Lens (Lens', lens)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Lumi.Components.Form (FormBuilder, Validated, Validator)
import Lumi.Components.Form as F
import Lumi.Components.LabeledField (RequiredField(..))

newtype EmailAddress = EmailAddress NonEmptyString

newtype Registration = Registration
  { email :: EmailAddress
  , password :: NonEmptyString
  }

type RegistrationFormData =
  { email :: Validated String
  , password :: Validated String
  }

registrationForm :: FormBuilder { readonly :: Boolean } RegistrationFormData Registration
registrationForm = ado
  email <-
    F.indent "Email" Required
    $ F.focus _email
    $ F.validated (isValidEmail "Email")
    $ F.validated (F.nonEmpty "Email")
    $ F.textbox
  password <-
    F.indent "Password" Required
    $ F.focus _password
    $ F.validated (F.nonEmpty "Password")
    $ F.passwordBox
  in
    Registration
      { email
      , password
      }

-- Utils {{{

_email :: forall a r. Lens' { email :: a | r } a
_email = lens _.email _{ email = _ }

_password :: forall a r. Lens' { password :: a | r } a
_password = lens _.password _{ password = _ }

isValidEmail :: String -> Validator NonEmptyString EmailAddress
isValidEmail name s =
  if NES.contains (NES.Pattern "@") s then
    Right $ EmailAddress s
  else
    Left $ name <> " is not a valid email address."

foreign import unsafeRegistrationToString :: Registration -> String

-- }}}
