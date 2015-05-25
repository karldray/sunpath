module Style where

import Color exposing (rgb)

midnightColor = rgb 50 50 50
darkColor = rgb 100 150 200
lightColor = rgb 255 230 150
noonColor = rgb 255 255 255
colorStopAngle = 0.2


label =
    [ ("display", "inline-block")
    , ("width", "60px")
    , ("text-align", "right")
    , ("color", "#666")
    , ("padding-right", "1ex")
    ]

input = [] {-
    [ ("height", "40px")
    , ("width", "200px")
    ]
-}
