module Model where

import Dict exposing (Dict)
import Array exposing (Array)

type alias Model =
    { levels: Array Level
    , state: GameState
    , current: Level
    , levelNumber: Int  -- starting from 0, but display = levelNumber + 1
    }

type Direction = Up | Down | Left | Right


type GameState = Menu | Playing | LevelCompleted | Victory


type alias Level =
    { map: Dict Position Tile
    , boxes: List Position
    , character: Position
    , width: Int
    , height: Int
    , moveCounter: Int
    , pushCounter: Int
    }


type alias Position = (Int, Int)

type Tile = Wall | Floor | Goal


isEmpty: Position -> Level -> Bool
isEmpty p lvl =
    let
        notWall = Dict.get p lvl.map /= Just Wall
    in
        notWall && not (isBox p lvl)


isBox: Position -> Level -> Bool
isBox position level =
    List.member position level.boxes

isGoal: Position -> Level -> Bool
isGoal position level =
    Dict.get position level.map == Just Goal

isCompleted: Level -> Bool
isCompleted level =
    List.all (\p -> isGoal p level) level.boxes


makeGame: List Level -> Maybe Model
makeGame levels =
    let
        make level =
            { levels = Array.fromList levels
            , state = Playing
            , current = level
            , levelNumber = 0
            }
    in
        case List.head levels of
            Just level -> Just (make level)
            Nothing -> Nothing


reset: Model -> Model
reset model =
    let
        initialLevel = Array.get model.levelNumber model.levels
    in
        case initialLevel of
            Just l -> { model | current <- l }

moveToNextLevel: Model -> Model
moveToNextLevel model =
    let
        initialLevel = Array.get (model.levelNumber + 1) model.levels
    in
        case initialLevel of
            Just l -> { model | current <- l, levelNumber <- model.levelNumber + 1 }
            Nothing -> { model | state <- Victory }


moveCharacter: Direction -> Level -> Level
moveCharacter direction level =
    let
        add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
        p = add level.character (getDelta direction)
        p' = add p (getDelta direction)
    in
        if | isEmpty p level -> { level | character <- p, moveCounter <- level.moveCounter + 1 }
           | (isEmpty p' level) && (isBox p level) ->
               { level | character <- p
                   , boxes <- p' :: (List.filter (\b -> b /= p) level.boxes)
                   , moveCounter <- level.moveCounter + 1
                   , pushCounter <- level.pushCounter + 1 }
           | otherwise -> level



getDelta: Direction -> (Int, Int)
getDelta direction = case direction of
   Up -> (0, -1)
   Down -> (0, 1)
   Left -> (-1, 0)
   Right -> (1, 0)
