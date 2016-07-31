module Styles exposing (..)

row : List (String, String)
row =
  [("display", "flex")]


rowColHead : List (String, String)
rowColHead =
  [ ("background-color", "eee")
  , ("width", "100px")
  , ("height", "100px")
  ]


rowHead : List (String, String)
rowHead =
  [ ("background-color", "eee")
  , ("width", "100px")
  , ("height", "30px")
  , ("line-height", "30px")
  ]


colHead : List (String, String)
colHead =
  [ ("background-color", "eee")
  , ("width", "30px")
  , ("height", "100px")
  ]


colHeadText : List (String, String)
colHeadText =
  [ ("transform", "rotate(90deg)")
  , ("line-height", "30px")
  ]


cell : List (String, String)
cell =
  [ ("width", "30px")
  , ("height", "30px")
  , ("line-height", "30px")
  , ("text-align", "center")
  ]
