module Styles exposing (..)


type CellState =
  Warning | Exists | Hovered | None Bool


headerBgColor : Bool -> String
headerBgColor hover =
  if hover then
    "#abc"
  else
    "#ddd"


cellBgColor : CellState -> String
cellBgColor state =
  case state of
    Exists -> "#6d4"
    Warning -> "#d64"
    Hovered -> "#bcd"
    None colored ->
      if colored then "#ddd" else "#eee"


cellWidth : Int
cellWidth = 30


px : number -> String
px num = toString num ++ "px"


row : Int -> Int -> List (String, String)
row labelLength length =
  [ ("display", "flex")
  , ("border-bottom", "solid 1px #fff")
  , ("width", px (labelLength + length * cellWidth))
  ]


rowColHead : Int -> List (String, String)
rowColHead labelLength =
  [ ("background-color", headerBgColor False)
  , ("width", px labelLength)
  , ("height", px labelLength)
  , ("box-sizing", "border-box")
  , ("border-right", "solid 1px #fff")
  ]


rowHead : Bool -> Int -> List (String, String)
rowHead hover labelLength =
  [ ("background-color", headerBgColor hover)
  , ("width", px labelLength)
  , ("height", px cellWidth)
  , ("line-height", px cellWidth)
  , ("box-sizing", "border-box")
  , ("border-right", "solid 1px #fff")
  , ("text-align", "right")
  , ("padding-right", "5px")
  ]


colHead : Bool -> Int -> List (String, String)
colHead hover labelLength =
  [ ("background-color", headerBgColor hover)
  , ("width", px cellWidth)
  , ("height", px labelLength)
  , ("box-sizing", "border-box")
  , ("border-right", "solid 1px #fff")
  ]


colHeadText : Int -> List (String, String)
colHeadText labelLength =
  [ ("transform", "rotate(-90deg) translate(-" ++ px labelLength ++ ")")
  , ("transform-origin", "top left")
  , ("width", px labelLength)
  , ("height", px cellWidth)
  , ("padding-left", "5px")
  , ("line-height", px cellWidth)
  ]


cell : CellState -> List (String, String)
cell state =
  [ ("width", px cellWidth)
  , ("height", px cellWidth)
  , ("line-height", px cellWidth)
  , ("text-align", "center")
  , ("background-color", cellBgColor state)
  , ("box-sizing", "border-box")
  , ("border-right", "solid 1px #fff")
  ]
