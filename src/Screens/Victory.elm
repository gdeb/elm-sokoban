module Screens.Victory where

import Graphics.Element exposing (show, Element)

import Input exposing (Input)
import Context exposing (Context)

-- model
type alias Model = ()


-- update
update: Input -> Model -> Model
update input model = model


-- view
view: Context -> Model -> Element
view context model =
    show "Congratulation! You solved every puzzle."
