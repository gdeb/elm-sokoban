module Screens.Menu where

import Color
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Text

import Context exposing (Context)
import Input exposing (KeyboardInput)

import Images.Title exposing (title)
import Images.Box exposing (box)

-- menu 
menu: List (String, Request)
menu =
    [ ("Start", StartGame)
    , ("Help", DisplayHelp)
    ]


-- model
type alias Model = Int


-- update
type Request = StartGame | DisplayHelp

update: KeyboardInput -> Model -> (Model, Maybe Request)
update input model =
    let
        dec s = (s - 1) % (List.length menu)
        inc s = (s + 1) % (List.length menu)
    in
        case input of
            Input.Down -> (inc model, Nothing)
            Input.Up -> (dec model, Nothing)
            Input.Enter ->
                case List.head (List.drop model menu) of
                    Just (_, request) -> (model, Just request)
            otherwise -> (model, Nothing)


-- view
view: Context -> Model -> Element
view context index =
    let
        decorate i (menu', _) = renderMenuLine context menu' (i == index)

        decoratedMenu =
            collage 300 200
                [ rect 300 200 |> filled Color.lightGray
                , menu |> List.indexedMap decorate |> flow down |> toForm
                ]


        title' =
            title context.width
    in
        flow down
            [ title'
            , decoratedMenu |> container context.width (context.height - (heightOf title')) middle

            ]


renderMenuLine: Context -> String -> Bool -> Element
renderMenuLine context text isActive =
    let
        menuText =
            Text.fromString text
                |> Text.height 20
                |> Text.color Color.charcoal
                |> centered

        decoration =
            if isActive then
                [ move (-80, 0) box
                , move (80, 0) box
                ]
            else
                []

        forms =
            List.append decoration [toForm menuText]
    in
        collage context.width 50 forms
