module DepCheck exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (programWithFlags)
import Set exposing (..)

import Styles as S


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


view : Model -> Html msg
view model =
  let
    set =
      createSet model.deps

    sorted =
      sortDeps set model.deps

    head =
      List.map fst sorted

    body =
      List.map (\(mod, _) ->
        (mod, List.map (\(imp, mine) ->
          Set.member (mod, imp) set
        ) sorted)
      ) sorted
  in
    div [] (headRowView head :: bodyView body)


headRowView : List String -> Html msg
headRowView imps =
  div [ style S.row ] (
    div [ style S.rowColHead ] [] ::
    List.map (\imp -> div [ style S.colHead ] [ div [ style S.colHeadText ] [ text imp ] ]) imps
  )


bodyView : List (String, List Bool) -> List (Html msg)
bodyView mods =
  List.map bodyRowView mods


bodyRowView : (String, List Bool) -> Html msg
bodyRowView (mod, imps) =
  div [ style S.row ] (
    div [ style S.rowHead ] [ text mod ] ::
    List.map cellView imps
  )


cellView : Bool -> Html msg
cellView imp =
  div [ style S.cell ] [ text (if imp then "1" else "0" ) ]


type alias Row = (String, List Bool)


sortDeps : Set (String, String) -> List (String, List (String, Bool)) -> List (String, List (String, Bool))
sortDeps set deps =
  let
    order (a, _) (b, _) =
      if Set.member (a, b) set then
        LT
      else
        GT
  in
    List.sortWith order deps


createSet : List (String, List (String, Bool)) -> Set (String, String)
createSet deps =
  List.foldl (\(mod, imps) set ->
    List.foldl (\(imp, mine) set ->
      (if mine then Set.insert (mod, imp) else identity) set
    ) set imps
  ) Set.empty deps
