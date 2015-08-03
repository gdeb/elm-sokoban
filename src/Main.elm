import Window
import Graphics.Element exposing (Element, show)

import Input exposing (input, Input)
import Context exposing (context, Context)
import Level
import LevelData exposing (levels)

import Screens.Game as G
import Screens.LevelCompleted as L
import Screens.Victory as V


initialModel: Signal Model
initialModel =
    case (List.head levels) of
        Just l ->
            let
                model = Playing { current = l, initial = l, levelNumber = 0 }
            in
                Signal.foldp update model input

main: Signal Element
main =
    Signal.map2 view context initialModel


-- model
type Model
    = Playing G.Model
    | LevelCompleted L.Model
    | Victory V.Model


-- update
update: Input -> Model -> Model
update input model = case model of
    Playing game -> updatePlaying input game
    LevelCompleted game -> updateLevelCompleted input game
    Victory game -> Victory (V.update input game)


updatePlaying: Input -> G.Model -> Model
updatePlaying input game =
    let
        (g, r) = G.update input game
    in case r of
        Just (G.LevelCompleted) -> LevelCompleted g
        Nothing -> Playing g


updateLevelCompleted: Input -> L.Model -> Model
updateLevelCompleted input game =
    let
        (_, r) =
            L.update input game

        loadLevel n = case List.head (List.drop n levels) of
            Just l -> Playing
                { current = l
                , initial = l
                , levelNumber = game.levelNumber + 1
                }
            Nothing -> Victory ()
    in
        case r of
            Just (L.LoadNextLevel) -> loadLevel (game.levelNumber + 1)
            Nothing -> LevelCompleted game

-- view
view: Context -> Model -> Element
view context model = case model of
    Playing game -> G.view context game
    LevelCompleted level -> L.view context level
    Victory m -> V.view context m
