module View where

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

type alias View = Context -> Model.Model -> Element


view: Context -> Model.Model -> Element
view context model = case model.state of
    Model.Playing -> render context model
    Model.Victory -> victory context model


render: View
render context model =
    let title =
        Text.fromString "Sokoban"
            |> Text.height 40
            |> Text.color Color.red
            |> centered
    in
        flow down
            [ title
            , (show ("Level " ++ (toString (model.levelNumber + 1))))
            , (show ("Moves " ++ (toString model.current.moveCounter)))
            , (show ("Pushes " ++ (toString model.current.pushCounter)))
            , drawLevel model.current
            ]
        |> container context.width context.height middle


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
