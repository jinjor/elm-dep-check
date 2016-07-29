module DepCheck exposing (..)

import Html exposing (..)
import Html.App exposing (programWithFlags)


type alias Flags =
  { deps : List (String, List (String, Bool))
  }


main : Program Flags
main = programWithFlags
  { init = init
  , update = update
  , subscriptions = always Sub.none
  , view = view
  }


type alias Model =
  { deps : List (String, List (String, Bool)) }


type Msg = NoOp


init : Flags -> (Model, Cmd Msg)
init flags =
  { deps = flags.deps } ! []


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  model ! []


view : model -> Html msg
view model =
  div [] [ text (toString model) ]
