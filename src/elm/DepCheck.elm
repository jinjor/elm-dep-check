module DepCheck exposing (..)

import Html exposing (..)
import Html.App exposing (programWithFlags)

import Model exposing (..)
import View

type alias Flags =
  { deps : List (String, List String)
  }


main : Program Flags
main = programWithFlags
  { init = \flags -> init flags.deps
  , update = update
  , subscriptions = always Sub.none
  , view = View.view
  }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    EnterCell tableName (rowIndex, colIndex) ->
      { model | hover = Just (tableName, (rowIndex, colIndex)) } ! []

    LeaveCell ->
      { model | hover = Nothing } ! []
