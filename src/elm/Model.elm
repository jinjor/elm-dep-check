module Model exposing (..)


type alias Model =
  { deps : List (String, List (String, Bool))
  , hover : Maybe (String, (Int, Int))
  }


type Msg
  = EnterCell String (Int, Int)
  | LeaveCell
