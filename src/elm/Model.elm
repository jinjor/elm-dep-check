module Model exposing (..)

import Set exposing (..)
import Dict exposing (..)
import String

import Graph exposing (..)


type alias Model =
  { deps : List (String, List (String, Bool))
  , dirLabelLength : Int
  , dirNames : List String
  , dirBody : List (String, List ((Int, Int), Bool))
  , modLabelLength : Int
  , groupedModNames : List String
  , modBody : List (String, List ((Int, Int), Bool))
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

    (directories, dirDeps) =
      createPackageDeps deps

    dirGraph =
      makePackageGraph (directories, dirDeps)

    dirNames =
      sort dirGraph

    groupedModNames =
      sortModuleAgain dirNames modNames

    dirIndexOf mod imp =
      ( indexOf (dirName mod) dirNames
      , indexOf (dirName imp) dirNames
      )

    modBody =
      List.map (\mod ->
        (mod, List.map (\imp ->
          (dirIndexOf mod imp, Set.member (mod, imp) set)
        ) groupedModNames)
      ) groupedModNames

    modLabelLength =
      (List.foldl Basics.max 0 (List.map String.length modNames)) * 15

    dirBody =
      List.map (\mod ->
        (mod, List.map (\imp ->
          ((0, 0), mod /= imp && Dict.member (mod, imp) dirDeps)
        ) dirNames)
      ) dirNames

    dirLabelLength =
      (List.foldl Basics.max 0 (List.map String.length dirNames)) * 15
  in
    { deps = deps
    , dirLabelLength = dirLabelLength
    , dirNames = dirNames
    , dirBody = dirBody
    , modLabelLength = modLabelLength
    , groupedModNames = groupedModNames
    , modBody = modBody
    , hover = Nothing
    } ! []


indexOf : a -> List a -> Int
indexOf a list =
  indexOfHelp 0 a list


indexOfHelp : Int -> a -> List a -> Int
indexOfHelp index a list =
  case list of
    [] -> -1
    x :: xs ->
      if a == x then
        index
      else
        indexOfHelp (index + 1) a xs


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
    directories =
      List.foldl (\(mod, _) dict ->
        let
          modPkg =
            dirName mod
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

    dirDeps =
      List.foldl (\(mod, imps) dict ->
        let
          modPkg =
            dirName mod
        in
          List.foldl
            (\(imp, mine) dict ->
              let
                impPkg =
                  dirName imp
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
    (directories, dirDeps)


dirName : String -> String
dirName modName =
  case List.reverse (String.split "." modName) of
    [] -> ""
    x :: [] -> ""
    x :: xs -> String.join "." (List.reverse xs)



sort : Graph String a -> List String
sort graph =
  List.map (\nodeContext -> nodeContext.node.label) (Graph.topologicalSort graph)



sortModuleAgain : List String -> List String -> List String
sortModuleAgain dirNames modNames =
  let
    dict =
      List.foldr
        (\mod dict ->
          let
            dir =
              dirName mod
          in
            Dict.update
              dir
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

    dirList =
      List.filterMap (flip Dict.get dict) dirNames

  in
    List.concatMap identity dirList


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
makePackageGraph (directories, deps) =
  let
    directoriesWithIndex =
      List.indexedMap (\id (dir, _) -> (id, dir)) (Dict.toList directories)

    dirToId =
      Dict.fromList (List.map (\(id, dir) -> (dir, id)) directoriesWithIndex)

    nodes =
      List.map (\(id, dir) -> Node id dir) directoriesWithIndex

    edges =
      List.filterMap
        (\((from, to), label) ->
          case (Dict.get from dirToId, Dict.get to dirToId) of
            (Just fromId, Just toId) ->
              Just (Edge fromId toId label)

            _ ->
              Nothing
        )
        (Dict.toList deps)
  in
    Graph.fromNodesAndEdges nodes edges
