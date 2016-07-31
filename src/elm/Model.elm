module Model exposing (..)


type alias Model =
  { deps : List (String, List (String, Bool)) }


type Msg = NoOp
