module Images.Character where

import Color
import Graphics.Collage exposing (..)

width: number
width = 30


character: Form
character =
    [ circle ((width / 2) - 3) |> filled (Color.blue)
    , circle ((width / 2) - 6) |> filled (Color.lightBlue)
    ]
        |> collage width width
        |> toForm
