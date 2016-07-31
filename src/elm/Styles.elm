module Styles exposing (..)


headerBgColor = "#ddd"


labelLength = "200px"


cellWidth = "30px"


row : List (String, String)
row =
  [ ("display", "flex")
  , ("margin-bottom", "1px")
  ]


rowColHead : List (String, String)
rowColHead =
  [ ("background-color", headerBgColor)
  , ("width", labelLength)
  , ("height", labelLength)
  , ("box-sizing", "border-box")
  ]


rowHead : List (String, String)
rowHead =
  [ ("background-color", headerBgColor)
  , ("width", labelLength)
  , ("height", cellWidth)
  , ("line-height", cellWidth)
  , ("box-sizing", "border-box")
  , ("margin-right", "1px")
  , ("text-align", "right")
  , ("padding-right", "5px")
  ]


colHead : List (String, String)
colHead =
  [ ("background-color", headerBgColor)
  , ("width", cellWidth)
  , ("height", labelLength)
  , ("box-sizing", "border-box")
  , ("margin-left", "1px")
  ]


colHeadText : List (String, String)
colHeadText =
  [ ("transform", "rotate(-90deg) translate(-" ++ labelLength ++ ")")
  , ("transform-origin", "top left")
  , ("width", labelLength)
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
