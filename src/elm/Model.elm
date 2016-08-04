module Model exposing (..)

import Set exposing (..)
import Dict exposing (..)
import String

import Graph exposing (..)


type alias Model =
  { deps : List (String, List (String, Bool))
  , pkgLabelLength : Int
  , pkgNames : List String
  , pkgBody : List (String, List Bool)
  , modLabelLength : Int
  , groupedModNames : List String
  , modBody : List (String, List Bool)
  , hover : Maybe (String, (Int, Int))
  }


type Msg
  = EnterCell String (Int, Int)
  | LeaveCell


init : List (String, List (String, Bool)) -> (Model, Cmd Msg)
init deps =
  let
    modGraph =
      makeModuleGraph deps

    set =
      createSet deps

    modNames =
      sort modGraph

    (packages, pkgDeps) =
      createPackageDeps deps

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

  in
    { deps = deps
    , pkgLabelLength = pkgLabelLength
    , pkgNames = pkgNames
    , pkgBody = pkgBody
    , modLabelLength = modLabelLength
    , groupedModNames = groupedModNames
    , modBody = modBody
    , hover = Nothing
    } ! []


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



sort : Graph String a -> List String
sort graph =
  List.map (\nodeContext -> nodeContext.node.label) (Graph.topologicalSort graph)



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
