module Screens.Menu where

import Color
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Text

import Context exposing (Context)
import Input exposing (KeyboardInput)
import Level

import Images.Title exposing (title)
import Images.Box exposing (box)

-- model
type Model = MainMenu MenuState | HelpMenu


type alias MenuState = Int

menu =
    [ ("Start", \m -> (m, Just Start))
    , ("Help", \m -> (HelpMenu, Nothing))
    ]

-- update
type Request = Start

update: KeyboardInput -> Model -> (Model, Maybe Request)
update input model =
    let
        dec s = (s - 1) % (List.length menu)
        inc s = (s + 1) % (List.length menu)
    in
        case (model, input) of
            (MainMenu s, Input.Down) -> (MainMenu (inc s), Nothing)
            (MainMenu s, Input.Up) -> (MainMenu (dec s), Nothing)
            (MainMenu s, Input.Enter) ->
                case List.head (List.drop s menu) of
                    Just (_, f) -> f model
            (HelpMenu, Input.Esc) -> (MainMenu 1, Nothing)
            otherwise -> (model, Nothing)


-- view
view: Context -> Model -> Element
view context model =
    case model of
        MainMenu s -> renderMainMenu context s
        HelpMenu -> renderHelpMenu context


renderMainMenu: Context -> Int -> Element
renderMainMenu context index =
    let
        decorate i (menu, _) = renderMenuLine context menu (i == index)

        decoratedMenu =
            menu
                |> List.indexedMap decorate
                |> flow down
                |> container context.width (context.height - (heightOf title')) middle

        title' =
            title context.width
    in
        flow down
            [ title'
            , decoratedMenu
            ]

renderHelpMenu: Context -> Element
renderHelpMenu context =
    flow down
        [ title context.width
        , show "help menu"
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
                [ rect 200 50 |> filled Color.gray
                , move (-80, 0) box
                , move (80, 0) box
                ]
            else
                []

        forms =
            List.append decoration [toForm menuText]
    in
        collage context.width 50 forms
