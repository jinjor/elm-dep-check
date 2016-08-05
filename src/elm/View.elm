module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (..)
import Set exposing (..)
import Dict exposing (..)
import String

import Graph exposing (..)

import Model exposing (..)
import Styles as S


view : Model -> Html Msg
view model =
    div
      []
      [ h2 [] [ text "Directories" ]
      , tableView
          model.hover
          "directories"
          model.dirLabelLength
          model.dirNames
          model.dirBody
      , h2 [] [ text "Modules" ]
      , tableView
          model.hover
          "modules"
          model.modLabelLength
          model.groupedModNames
          model.modBody
      ]


tableView : Maybe (String, (Int, Int)) -> String -> Int -> List String -> List (String, List ((Int, Int), Bool)) -> Html Msg
tableView hoverState tableName labelLength names body =
  div []
    ( headRowView (isHovered hoverState) tableName labelLength names ::
      bodyView (isHovered hoverState) tableName labelLength body
    )


isHovered : Maybe (String, (Int, Int)) -> String -> (Int, Int) -> Bool
isHovered hoverState tableName (rowIndex, colIndex) =
  case hoverState of
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


headRowView : (String -> (Int, Int) -> Bool) -> String -> Int -> List String -> Html Msg
headRowView isHovered tableName labelLength imps =
  Keyed.node "div" [ style (S.row labelLength (List.length imps)) ] (
    ("cell-head-head", rowColheadView labelLength) ::
    List.indexedMap (\colIndex imp ->
      let
        state =
          isHovered tableName (-1, colIndex)

        key =
          "cell-head-" ++ toString colIndex

        html =
          div
            [ style (S.colHead state labelLength)
            , onMouseEnter (EnterCell tableName (-1, colIndex))
            , onMouseLeave LeaveCell
            ]
            [ div [ style (S.colHeadText labelLength) ] [ text imp ]
            ]
      in
        (key, html)
    ) imps
  )


rowColheadView : Int -> Html msg
rowColheadView labelLength =
  div [ style (S.rowColHead labelLength) ] []


bodyView : (String -> (Int, Int) -> Bool) -> String -> Int -> List (String, List ((Int, Int), Bool)) -> List (Html Msg)
bodyView isHovered tableName labelLength mods =
  List.indexedMap
    (bodyRowView isHovered tableName labelLength (List.length mods))
    mods


bodyRowView : (String -> (Int, Int) -> Bool) -> String -> Int -> Int -> Int -> (String, List ((Int, Int), Bool)) -> Html Msg
bodyRowView isHovered tableName labelLength length rowIndex (mod, imps) =
  let
    head =
      ( "cell-" ++ toString rowIndex ++ "-head"
      , bodyRowHeadView isHovered tableName labelLength rowIndex mod
      )

    tail =
      List.indexedMap
        (cellView isHovered tableName rowIndex)
        imps
  in
    Keyed.node "div" [ style (S.row labelLength length) ] (head :: tail)


bodyRowHeadView : (String -> (Int, Int) -> Bool) -> String -> Int -> Int -> String -> Html Msg
bodyRowHeadView isHovered tableName labelLength rowIndex mod =
  div
    [ style (S.rowHead (isHovered tableName (rowIndex, -1)) labelLength)
    , onMouseEnter (EnterCell tableName (rowIndex, -1))
    , onMouseLeave LeaveCell
    ] [ text mod ]


cellView : (String -> (Int, Int) -> Bool) -> String -> Int -> Int -> ((Int, Int), Bool) -> (String, Html Msg)
cellView isHovered tableName rowIndex colIndex ((dirY, dirX), exists) =
  let
    cellState =
      if exists && rowIndex > colIndex then
        S.Warning
      else if exists then
        S.Exists
      else if isHovered tableName (rowIndex, colIndex) then
        S.Hovered
      else
        S.None ((dirX + dirY) % 2 == 1)

    key =
      "cell-" ++ toString rowIndex ++ "-" ++ toString colIndex

    text =
      if exists then "1" else "0"

    html =
      lazy3 cellViewHelp cellState tableName (rowIndex, colIndex, text)
  in
    (key, html)


cellViewHelp : S.CellState -> String -> (Int, Int, String) -> Html Msg
cellViewHelp cellState tableName (rowIndex, colIndex, s) =
  div
    [ style (S.cell cellState)
    , onMouseEnter (EnterCell tableName (rowIndex, colIndex))
    , onMouseLeave LeaveCell
    ]
    [ text s ]
