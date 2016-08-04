module Styles exposing (..)


type CellState =
  Warning | Exists | Hovered | None


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
    None -> "#eee"


cellWidth : String
cellWidth = "30px"


px : number -> String
px num = toString num ++ "px"


row : List (String, String)
row =
  [ ("display", "flex")
  , ("margin-bottom", "1px")
  ]


rowColHead : Int -> List (String, String)
rowColHead labelLength =
  [ ("background-color", headerBgColor False)
  , ("width", px labelLength)
  , ("height", px labelLength)
  , ("box-sizing", "border-box")
  ]


rowHead : Bool -> Int -> List (String, String)
rowHead hover labelLength =
  [ ("background-color", headerBgColor hover)
  , ("width", px labelLength)
  , ("height", cellWidth)
  , ("line-height", cellWidth)
  , ("box-sizing", "border-box")
  , ("margin-right", "1px")
  , ("text-align", "right")
  , ("padding-right", "5px")
  ]


colHead : Bool -> Int -> List (String, String)
colHead hover labelLength =
  [ ("background-color", headerBgColor hover)
  , ("width", cellWidth)
  , ("height", px labelLength)
  , ("box-sizing", "border-box")
  , ("margin-left", "1px")
  ]


colHeadText : Int -> List (String, String)
colHeadText labelLength =
  [ ("transform", "rotate(-90deg) translate(-" ++ px labelLength ++ ")")
  , ("transform-origin", "top left")
  , ("width", px labelLength)
  , ("height", cellWidth)
  , ("padding-left", "5px")
  , ("line-height", cellWidth)
  ]


cell : CellState -> List (String, String)
cell state =
  [ ("width", cellWidth)
  , ("height", cellWidth)
  , ("line-height", cellWidth)
  , ("text-align", "center")
  , ("background-color", cellBgColor state)
  , ("box-sizing", "border-box")
  , ("margin-right", "1px")
  ]
