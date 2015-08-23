module Images.Box where

import Color
import Graphics.Collage exposing (..)

width: number
width = 30

box: Form
box =
    [ rect (width - 3) (width - 3) |> filled Color.orange
    , rect (width - 10) (width - 10) |> filled Color.lightOrange
    ]
        |> collage width width
        |> toForm
