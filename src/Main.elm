import Graphics.Element exposing (Element, show)

import Input exposing (input, KeyboardInput)
import Context exposing (context, Context)
import LevelData exposing (levels)
import Screens.Game as G
import Screens.LevelCompleted as L
import Screens.Victory as V


main: Signal Element
main =
    let
        initialModel: Signal Model
        initialModel = case (List.head levels) of
            Just l ->
                let
                    model = Playing { current = l, initial = l, levelNumber = 0 }
                in
                    Signal.foldp update model input
    in
        Signal.map2 view context initialModel


-- model
type Model
    = Playing G.Model
    | LevelCompleted L.Model
    | Victory V.Model


-- update
update: KeyboardInput -> Model -> Model
update input model = case model of
    Playing game -> updatePlaying input game
    LevelCompleted game -> updateLevelCompleted input game
    Victory game -> Victory (V.update input game)


updatePlaying: KeyboardInput -> G.Model -> Model
updatePlaying input game =
    let
        (g, r) = G.update input game
    in
        case r of
            Just (G.LevelCompleted) -> LevelCompleted g
            Nothing -> Playing g


updateLevelCompleted: KeyboardInput -> L.Model -> Model
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
