module Images.Floor where

import Color
import Graphics.Collage exposing (..)

width: number
width = 30

floor': Form
floor' =
    rect width width
        |> filled (Color.rgba 191 191 191 0.6)
