module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Set exposing (..)
import Dict exposing (..)
import String

import Graph exposing (..)

import Model exposing (..)
import Styles as S


view : Model -> Html Msg
view model =
  let
    modGraph =
      makeModuleGraph model.deps

    set =
      createSet model.deps

    modNames =
      sort modGraph

    (packages, pkgDeps) =
      createPackageDeps model.deps

    pkgGraph =
      makePackageGraph (packages, pkgDeps)

    pkgNames =
      sort pkgGraph

    groupedModNames =
      sortModuleAgain pkgNames modNames

    modBody =
      List.map (\mod ->
        (mod, List.map (\imp ->
          Set.member (mod, imp) set
        ) groupedModNames)
      ) groupedModNames

    modLabelLength =
      (List.foldl Basics.max 0 (List.map String.length modNames)) * 15

    pkgBody =
      List.map (\mod ->
        (mod, List.map (\imp ->
          mod /= imp && Dict.member (mod, imp) pkgDeps
        ) pkgNames)
      ) pkgNames

    pkgLabelLength =
      (List.foldl Basics.max 0 (List.map String.length pkgNames)) * 15

    isHovered tableName (rowIndex, colIndex) =
      case model.hover of
        Just (t, (r, c)) ->
          t == tableName &&
            if rowIndex == -1 then
              c == colIndex
            else if colIndex == -1 then
              r == rowIndex
            else
              r == rowIndex || c == colIndex

        Nothing ->
          False
  in
    div
      []
      [ div []
        ( headRowView isHovered "packages" pkgLabelLength pkgNames ::
          bodyView isHovered "packages" pkgLabelLength pkgBody
        )
      , hr [] []
      , div []
        ( headRowView isHovered "modules" modLabelLength groupedModNames ::
          bodyView isHovered "modules" modLabelLength modBody
        )
      ]


sortModuleAgain : List String -> List String -> List String
sortModuleAgain pkgNames modNames =
  let
    dict =
      List.foldr
        (\mod dict ->
          let
            pkg =
              pkgName mod
          in
            Dict.update
              pkg
              (\value ->
                case value of
                  Just list ->
                    Just (mod :: list)

                  Nothing ->
                    Just []
              )
              dict
        )
        Dict.empty
        modNames

    pkgList =
      List.filterMap (flip Dict.get dict) pkgNames

  in
    List.concatMap identity pkgList


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


headRowView : (String -> (Int, Int) -> Bool) -> String -> Int -> List String -> Html Msg
headRowView isHovered tableName labelLength imps =
  div [ style S.row ] (
    div [ style (S.rowColHead labelLength) ] [] ::
    List.indexedMap (\colIndex imp ->
      let
        state =
          isHovered tableName (-1, colIndex)
      in
        div
          [ style (S.colHead state labelLength)
          , onMouseEnter (EnterCell tableName (-1, colIndex))
          , onMouseLeave LeaveCell
          ]
          [ div [ style (S.colHeadText labelLength) ] [ text imp ] ]
    ) imps
  )


bodyView : (String -> (Int, Int) -> Bool) -> String -> Int -> List (String, List Bool) -> List (Html Msg)
bodyView isHovered tableName labelLength mods =
  List.indexedMap (bodyRowView isHovered tableName labelLength) mods


bodyRowView : (String -> (Int, Int) -> Bool) -> String -> Int -> Int -> (String, List Bool) -> Html Msg
bodyRowView isHovered tableName labelLength rowIndex (mod, imps) =
  div [ style S.row ] (
    div
      [ style (S.rowHead (isHovered tableName (rowIndex, -1)) labelLength)
      , onMouseEnter (EnterCell tableName (rowIndex, -1))
      , onMouseLeave LeaveCell
      ] [ text mod ] ::
    List.indexedMap (cellView isHovered tableName rowIndex) imps
  )


cellView : (String -> (Int, Int) -> Bool) -> String -> Int -> Int -> Bool -> Html Msg
cellView isHovered tableName rowIndex colIndex imp =
  let
    cellOption =
      if imp && rowIndex > colIndex then
        S.Warning
      else if imp then
        S.Exists
      else if isHovered tableName (rowIndex, colIndex) then
        S.Hovered
      else
        S.None
  in
    div
      [ style (S.cell cellOption)
      , onMouseEnter (EnterCell tableName (rowIndex, colIndex))
      , onMouseLeave LeaveCell
      ]
      [ text (if imp then "1" else "0" ) ]


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
                (if mine && modPkg /= impPkg then
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
