module Level where

import Dict exposing (Dict)
import Color
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

import Images.Character exposing (character)
import Images.Box exposing (box)
import Images.Floor exposing (floor')

-- model
type alias Model =
    { map: Dict Position Tile
    , boxes: List Position
    , character: Position
    , width: Int
    , height: Int
    , moveCounter: Int
    , pushCounter: Int
    }


type Tile = Wall | Floor | Goal


type alias Position = (Int, Int)


-- update
type Action = MoveLeft | MoveRight | MoveUp | MoveDown

update: Action -> Model -> Model
update action model =
    let
        add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
        p = add model.character (getDelta action)
        p' = add p (getDelta action)
    in
        if | isEmpty p model -> { model | character <- p, moveCounter <- model.moveCounter + 1 }
           | (isEmpty p' model) && (isBox p model) ->
               { model | character <- p
                   , boxes <- p' :: (List.filter (\b -> b /= p) model.boxes)
                   , moveCounter <- model.moveCounter + 1
                   , pushCounter <- model.pushCounter + 1 }
           | otherwise -> model



getDelta: Action -> (Int, Int)
getDelta action = case action of
  MoveUp -> (0, -1)
  MoveDown -> (0, 1)
  MoveLeft -> (-1, 0)
  MoveRight -> (1, 0)

isEmpty: Position -> Model -> Bool
isEmpty p lvl =
  let
      notWall = Dict.get p lvl.map /= Just Wall
  in
      notWall && not (isBox p lvl)


isBox: Position -> Model -> Bool
isBox position level =
    List.member position level.boxes


-- view
view: Model -> Element
view level =
    let
        getX x = (toFloat x * width) - (toFloat level.width) * width / 2 + width / 2
        getY y = (toFloat (level.height - y) * width - (toFloat level.height) * width / 2) - width / 2

        drawtile ((x, y), tile) =
            move (getX x, getY y) (drawTile tile)

        tiles = List.map drawtile (Dict.toList level.map)

        boxes = List.map (\(x,y) -> move (getX x, getY y) box) level.boxes

        (cx, cy) = level.character

        char = [ move (getX cx, getY cy) character ]
    in
        collage (level.width * width) (level.height * width) (List.concat [tiles, boxes, char])



drawTile: Tile -> Form
drawTile tile = case tile of
    Wall -> coloredRect Color.charcoal
    Floor -> floor'
    Goal -> collage width width
        [ coloredRect clearGrey
        , filled (Color.rgb 230 151 141) (circle (width / 6))
        ] |> toForm


width = 30

clearGrey = Color.rgba 191 191 191 0.6

makeTile shapes = collage (floor width) (floor width) shapes

coloredRect color = filled color (rect width width)
