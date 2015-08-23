module Screens.MainMenu where

import Graphics.Element exposing (..)

import Context exposing (Context)
import Input exposing (KeyboardInput)

import Images.Title exposing (title)
import Menu

-- menu 
menu: List (String, Request)
menu =
    [ ("Start", StartGame)
    , ("Help", DisplayHelp)
    ]


-- model
type alias Model = Menu.Model


-- update
type Request = StartGame | DisplayHelp

update: KeyboardInput -> Model -> (Model, Maybe Request)
update input model =
    let
        update' = Menu.update menu
    in
        case input of
            Input.Down -> update' Menu.Down model
            Input.Up -> update' Menu.Up model
            Input.Enter -> update' Menu.Select model
            otherwise -> (model, Nothing)


-- view
view: Context -> Model -> Element
view context index =
    let
        decoratedMenu = 
            Menu.view menu index

        title' =
            title context.width
    in
        flow down
            [ title'
            , decoratedMenu |> container context.width (context.height - (heightOf title')) middle
            ]
