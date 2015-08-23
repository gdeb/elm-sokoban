module Screens.Help where

import Graphics.Element exposing (show, Element, flow, down)

import Context exposing (Context)
import Input exposing (KeyboardInput)
import Images.Title exposing (title)


-- model
-- hack because compiler crash with type alias A a = a
type alias Model a = (a, a)


newModel: a -> Model a
newModel a = (a, a)

from: Model a -> a
from (a, a) = a


-- update
type Request = GoBack

update: KeyboardInput -> Model a -> (Model a, Maybe Request)
update input model = (model, Just GoBack)


-- view
view: Context -> Model a -> Element
view context model =
    flow down
        [ title context.width
        , show "help menu"
        ]
