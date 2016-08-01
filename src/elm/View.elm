module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Set exposing (..)
import Dict

import Graph exposing (..)

import Model exposing (..)
import Styles as S


view : Model -> Html msg
view model =
  let
    graph =
      makeGraph model.deps

    set =
      createSet model.deps

    modNames =
      sort graph

    body =
      List.map (\mod ->
        (mod, List.map (\imp ->
          Set.member (mod, imp) set
        ) modNames)
      ) modNames
  in
    div [] (headRowView modNames :: bodyView body)


makeGraph : List (String, List (String, Bool)) -> Graph String ()
makeGraph deps =
  let
    depsWithId =
      List.indexedMap (,) deps

    dict =
      Dict.fromList <|
        List.map (\(id, (mod, _)) -> (mod, id)) depsWithId

    (nodes, edges) =
      List.foldl
        (\(id, (mod, imps)) (nodes, edges) ->
          ( Node id mod :: nodes
          , List.filterMap (\(imp, _) ->
                case Dict.get imp dict of
                  Just impId -> Just (Edge id impId ())
                  Nothing -> Nothing
            ) imps ++ edges
          )
        )
        ([], [])
        depsWithId
  in
    Graph.fromNodesAndEdges nodes edges


sort : Graph String () -> List String
sort graph =
  List.map (\nodeContext -> nodeContext.node.label) (Graph.topologicalSort graph)


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
  div [ style (S.cell imp) ] [ text (if imp then "1" else "0" ) ]


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
