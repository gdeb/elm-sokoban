module Images.Title where

import Color
import Text
import Graphics.Element exposing (..)

-- import Graphics.Collage exposing (..)


title: Int -> Element
title width =
    Text.fromString "Elm-Sokoban"
        |> Text.height 40
        |> Text.color Color.red
        |> centered
        |> container width 80 middle
        |> color Color.gray
