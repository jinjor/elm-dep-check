module Model exposing (..)

import Set exposing (..)
import Dict exposing (..)
import String

import Graph exposing (..)


type alias Model =
  { deps : List (String, List String)
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


init : List (String, List String) -> (Model, Cmd Msg)
init deps =
  let
    modGraph =
      makeModuleGraph deps

    modRelation =
      makeModRelation deps

    modNames =
      sort modGraph

    (directories, dirDeps) =
      makeDirectoryDeps deps

    dirGraph =
      makeDirectoryGraph (directories, dirDeps)

    dirNames =
      sort dirGraph

    groupedModNames =
      sortModuleAgain dirNames modNames

    modBody =
      makeModBody dirNames modRelation groupedModNames

    modLabelLength =
      maxPxLengthOfNames modNames

    dirBody =
      makeDirBody dirDeps dirNames

    dirLabelLength =
      maxPxLengthOfNames dirNames
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


maxPxLengthOfNames : List String -> Int
maxPxLengthOfNames names =
  maxLengthOfNames names * 15


maxLengthOfNames : List String -> Int
maxLengthOfNames names =
  List.foldl Basics.max 0 (List.map String.length names)


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


makeModBody : List String -> Set (String, String) -> List String -> List (String, List ((Int, Int), Bool))
makeModBody dirNames modRelation modNames =
  let
    dirIndexOf mod imp =
      ( indexOf (dirName mod) dirNames
      , indexOf (dirName imp) dirNames
      )
  in
    List.map (\mod ->
      (mod, List.map (\imp ->
        (dirIndexOf mod imp, Set.member (mod, imp) modRelation)
      ) modNames)
    ) modNames


makeDirBody : Dict (String, String) (List (String, String)) -> List String -> List (String, List ((Int, Int), Bool))
makeDirBody dirDeps dirNames =
  List.map (\mod ->
    (mod, List.map (\imp ->
      ((0, 0), mod /= imp && Dict.member (mod, imp) dirDeps)
    ) dirNames)
  ) dirNames


makeModRelation : List (String, List String) -> Set (String, String)
makeModRelation deps =
  let
    modSet =
      Set.fromList (List.map fst deps)

  in
    Set.fromList <|
      List.concatMap
        (\(mod, imps) ->
          List.filterMap
            (\imp ->
              if Set.member imp modSet then
                Just (mod, imp)
              else
                Nothing
            )
            imps
        )
        deps


makeDirectoryDeps : List (String, List String)
  -> ( Dict String (List String)
     , Dict (String, String) (List (String, String))
     )
makeDirectoryDeps deps =
  let
    modSet =
      Set.fromList (List.map fst deps)

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
          modDir =
            dirName mod
        in
          List.foldl
            (\imp dict ->
              let
                impDir =
                  dirName imp
              in
                (if Set.member imp modSet && modDir /= impDir then
                  Dict.update
                    (modDir, impDir)
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
                    Just [mod]
              )
              dict
        )
        Dict.empty
        modNames

    dirList =
      List.filterMap (flip Dict.get dict) dirNames

  in
    List.concatMap identity dirList


makeModuleGraph : List (String, List String) -> Graph String ()
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
          , List.filterMap (\imp ->
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


makeDirectoryGraph :
    ( Dict String (List String)
    , Dict (String, String) (List (String, String))
    )
  -> Graph String (List (String, String))
makeDirectoryGraph (directories, deps) =
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
