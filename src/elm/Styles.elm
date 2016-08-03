module Styles exposing (..)


headerBgColor = "#ddd"


-- labelLength = "200px"

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
  [ ("background-color", headerBgColor)
  , ("width", px labelLength)
  , ("height", px labelLength)
  , ("box-sizing", "border-box")
  ]


rowHead : Int -> List (String, String)
rowHead labelLength =
  [ ("background-color", headerBgColor)
  , ("width", px labelLength)
  , ("height", cellWidth)
  , ("line-height", cellWidth)
  , ("box-sizing", "border-box")
  , ("margin-right", "1px")
  , ("text-align", "right")
  , ("padding-right", "5px")
  ]


colHead : Int -> List (String, String)
colHead labelLength =
  [ ("background-color", headerBgColor)
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


cell : Bool -> List (String, String)
cell exists =
  [ ("width", cellWidth)
  , ("height", cellWidth)
  , ("line-height", cellWidth)
  , ("text-align", "center")
  , ("background-color", if exists then "#6d4" else "#eee")
  , ("box-sizing", "border-box")
  , ("margin-right", "1px")
  ]
