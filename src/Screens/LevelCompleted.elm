module Screens.LevelCompleted where

import Color
import Graphics.Element exposing (show, Element)
import Graphics.Collage exposing (collage, alpha, toForm, filled, rect, text)
import Text

import Context exposing (Context)
import Input exposing (KeyboardInput)
import Screens.Game as G


-- model
type alias Model = G.Model


-- update
type Request = LoadNextLevel

update: KeyboardInput -> Model -> (Model, Maybe Request)
update input model = (model, Just LoadNextLevel)


-- view
view: Context -> Model -> Element
view context model =
    collage context.width context.height
        [ alpha 0.3 (toForm (G.view context model))
        , filled Color.gray (rect 324 74)
        , filled Color.white (rect 320 70)
        , text (Text.fromString "Level Completed" |> Text.height 40)
        ]
