module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Set exposing (..)
import Dict exposing (..)
import String

import Graph exposing (..)

import Model exposing (..)
import Styles as S


view : Model -> Html msg
view model =
  let
    modGraph =
      makeModuleGraph model.deps

    set =
      createSet model.deps

    modNames =
      sort modGraph

    pkgGraph =
      makePackageGraph
        (createPackageDeps model.deps)

    pkgNames =
      sort pkgGraph

    body =
      List.map (\mod ->
        (mod, List.map (\imp ->
          Set.member (mod, imp) set
        ) pkgNames)
      ) pkgNames
  in
    div [] (headRowView pkgNames :: bodyView body)


makeModuleGraph : List (String, List (String, Bool)) -> Graph String ()
makeModuleGraph deps =
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


makePackageGraph :
    ( Dict String (List String)
    , Dict (String, String) (List (String, String))
    )
  -> Graph String (List (String, String))
makePackageGraph (packages, deps) =
  let
    packagesWithIndex =
      List.indexedMap (\id (pkg, _) -> (id, pkg)) (Dict.toList packages)

    pkgToId =
      Dict.fromList (List.map (\(id, pkg) -> (pkg, id)) packagesWithIndex)

    nodes =
      List.map (\(id, pkg) -> Node id pkg) packagesWithIndex

    edges =
      List.filterMap
        (\((from, to), label) ->
          case (Dict.get from pkgToId, Dict.get to pkgToId) of
            (Just fromId, Just toId) ->
              Just (Edge fromId toId label)

            _ ->
              Nothing
        )
        (Dict.toList deps)
  in
    Graph.fromNodesAndEdges nodes edges



sort : Graph String a -> List String
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


createSet : List (String, List (String, Bool)) -> Set (String, String)
createSet deps =
  List.foldl (\(mod, imps) set ->
    List.foldl (\(imp, mine) set ->
      (if mine then Set.insert (mod, imp) else identity) set
    ) set imps
  ) Set.empty deps


createPackageDeps : List (String, List (String, Bool))
  -> ( Dict String (List String)
     , Dict (String, String) (List (String, String))
     )
createPackageDeps deps =
  let
    packages =
      List.foldl (\(mod, _) dict ->
        let
          modPkg =
            pkgName mod
        in
          Dict.update
            modPkg
            (\value ->
              case value of
                Just list -> Just (mod :: list)
                Nothing -> Just [ mod ]
            )
            dict
      ) Dict.empty deps

    pkgDeps =
      List.foldl (\(mod, imps) dict ->
        let
          modPkg =
            pkgName mod
        in
          List.foldl
            (\(imp, mine) dict ->
              let
                impPkg =
                  pkgName imp
              in
                (if mine then
                  Dict.update
                    (modPkg, impPkg)
                    (\value ->
                      case value of
                        Just list -> Just ((mod, imp) :: list)
                        Nothing -> Just [(mod, imp)]
                    )
                  else
                    identity
                ) dict
            ) dict imps
      ) Dict.empty deps
  in
    (packages, pkgDeps)


pkgName : String -> String
pkgName modName =
  case List.reverse (String.split "." modName) of
    [] -> ""
    x :: [] -> ""
    x :: xs -> String.join "." (List.reverse xs)
