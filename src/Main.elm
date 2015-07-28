import Window
import Graphics.Element exposing (Element, show)

import Levels
import Model exposing (Model, makeGame)
import View exposing (view)
import Update exposing (update)
import Input exposing (input)

context: Signal View.Context
context =
    Signal.map (\(w, h) -> {width = w, height = h}) Window.dimensions


initialModel: Maybe (Signal Model)
initialModel =
    case (makeGame Levels.levels) of
        Just model -> Just (Signal.foldp update model input)
        Nothing -> Nothing


main: Signal Element
main =
    case initialModel of
        Just model -> Signal.map2 view context model
        Nothing -> Signal.constant (show "Error: No levels available")
