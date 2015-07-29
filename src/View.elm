module View (view, Context) where

import Model

import Color
import Text
import Dict
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

type alias Context =
    { width: Int
    , height: Int
    }

width = 30


view: Context -> Model.Model -> Element
view context model = case model.state of
    Model.Playing -> render context model
    Model.LevelCompleted -> levelCompleted context model
    Model.Victory -> victory context model


render: Context -> Model.Model -> Element
render context model =
    let
        makeText str =
            Text.fromString str |> Text.height 20 |> Text.color Color.charcoal |> centered

        title =
            Text.fromString "Sokoban"
                |> Text.height 40
                |> Text.color Color.red
                |> centered
                |> container context.width 80 middle
                |> color Color.gray

        helpMessage =
            Text.fromString "arrows: move character, escape: reset level  "
                |> Text.height 15
                |> Text.color Color.charcoal
                |> centered
                |> container context.width 20 midRight

        currentLevel =
            drawLevel model.current
                |> container context.width (context.height - 140) middle

        statusBar =
            flow right
                [ makeText ("Level: " ++ (toString (model.levelNumber + 1)))
                , spacer 40 20
                , makeText ("Moves: " ++ (toString model.current.moveCounter))
                , spacer 40 20
                , makeText ("Pushes: " ++ (toString model.current.pushCounter))
                ]
                |> container context.width 40 middle |> color Color.gray
    in
        flow down
            [ title
            , helpMessage
            , currentLevel
            , statusBar
            ]


drawTile: Model.Tile -> Form
drawTile tile = case tile of
    Model.Wall -> coloredRect Color.charcoal
    Model.Floor -> coloredRect clearGrey
    Model.Goal -> collage width width
        [ coloredRect clearGrey
        , filled (Color.rgb 230 151 141) (circle (width / 6))
        ] |> toForm

box: Form
box = collage width width
    [ rect (width - 3) (width - 3) |> filled Color.orange
    , rect (width - 10) (width - 10) |> filled Color.lightOrange
    ] |> toForm

charForm: Form
charForm = collage width width
    [ circle ((width / 2) - 3) |> filled (Color.blue)
    , circle ((width / 2) - 6) |> filled (Color.lightBlue)
    ] |> toForm


levelCompleted: Context -> Model.Model -> Element
levelCompleted context model =
    collage context.width context.height
        [ alpha 0.4 (toForm (render context model))
        , filled Color.white (rect 300 50)
        , text (Text.fromString "Level Completed" |> Text.height 40)
        ]

-- helpers
clearGrey = Color.rgba 191 191 191 0.6

makeTile shapes = collage (floor width) (floor width) shapes

coloredRect color = filled color (rect width width)


drawLevel: Model.Level -> Element
drawLevel level =
    let
        getX x = (toFloat x * width) - (toFloat level.width) * width / 2 + width / 2
        getY y = (toFloat (level.height - y) * width - (toFloat level.height) * width / 2) - width / 2

        drawtile ((x, y), tile) =
            move (getX x, getY y) (drawTile tile)

        tiles = List.map drawtile (Dict.toList level.map)

        boxes = List.map (\(x,y) -> move (getX x, getY y) box) level.boxes

        (cx, cy) = level.character

        character = [ move (getX cx, getY cy) charForm ]
    in
        collage (level.width * width) (level.height * width) (List.concat [tiles, boxes, character])

victory: Context -> Model.Model -> Element
victory context model =
    show "Victory"
