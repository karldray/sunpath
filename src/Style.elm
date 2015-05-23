module Style where

import Color exposing (rgb)

midnightColor = rgb 75 75 75
darkColor = rgb 100 125 230
lightColor = rgb 200 180 50
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
