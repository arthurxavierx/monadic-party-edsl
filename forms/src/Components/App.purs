module Components.App where

import Prelude

import Components.Form (RegistrationFormData, unsafeRegistrationToString, registrationForm)
import Data.Lens (lens, over)
import Data.Maybe (Maybe(..))
import Lumi.Components.Form as F
import Lumi.Components.Row (row_)
import Lumi.Components.Spacing (Space(..), hspace)
import React.Basic (JSX, createComponent, make)
import React.Basic.DOM as R
import Record (disjointUnion)

app :: JSX
app =
  unit # make (createComponent "App")
    { initialState:
        { formData: F.formDefaults :: RegistrationFormData
        }
    , render
    }
  where
    formComponent = F.build registrationForm
    formProps =
      { readonly: false
      }

    render { state, setState } =
      row_
        [ R.div
            { style: R.css { width: "400px" }
            , children:
                [ formComponent $ disjointUnion formProps
                    { value: state.formData
                    , onChange: setState <<< over _formData
                    , forceTopLabels: false
                    , inlineTable: false
                    }
                ]
            }
        , hspace S12
        , R.pre
            { style: R.css { flex: "1" }
            , children:
                [ R.text
                    case F.revalidate registrationForm formProps state.formData of
                      Nothing ->
                        "Invalid form"
                      Just result ->
                        unsafeRegistrationToString result
                ]
            }
        ]

    _formData = lens _.formData _{ formData = _ }
