import Graphics.Element exposing (Element, show)

import Input exposing (input, KeyboardInput)
import Context exposing (context, Context)
import LevelData exposing (levels)
import Screens.Game as G
import Screens.LevelCompleted as L
import Screens.Menu as M
import Screens.Victory as V
import Screens.Help as H


-- main
main: Signal Element
main =
    let
        model = 
            Menu 0 
        
        initialModel =
            Signal.foldp update model input
    in
        Signal.map2 view context initialModel


-- model
type Model
    = Menu M.Model
    | HelpScreen (H.Model Model)
    | Playing G.Model
    | LevelCompleted L.Model
    | Victory V.Model


-- update
update: KeyboardInput -> Model -> Model
update input model = case model of
    Menu menu ->
        let
            (g, r) = M.update input menu
        in
            case r of
                Just (M.StartGame) ->
                    case (List.head levels) of
                        Just l -> Playing { current = l, initial = l, levelNumber = 0 }
                Just (M.DisplayHelp) ->
                    HelpScreen (H.newModel model)
                Nothing -> Menu g

    HelpScreen model' ->
        let
            (g, r) = H.update input model'
        in
            case r of
                Just (H.GoBack) -> H.from model'
                Nothing -> model
        
    Playing game -> 
        let
            (g, r) = G.update input game
        in
            case r of
                Just (G.LevelCompleted) -> LevelCompleted g
                Nothing -> Playing g
                
    LevelCompleted game -> 
        let 
            (g, r) = L.update input game
        in
            case r of
                Just (L.LoadNextLevel) ->
                    case (loadLevel (game.levelNumber + 1)) of
                        Just playing -> playing
                        Nothing -> Victory ()
                Nothing -> LevelCompleted game
            
    Victory game -> Victory (V.update input game)

    
loadLevel: Int -> Maybe Model
loadLevel n =
    Maybe.map
        (\l -> Playing { current = l, initial = l, levelNumber = n})
        (List.head (List.drop n levels))


-- view
view: Context -> Model -> Element
view context model = case model of
    Menu menu -> M.view context menu
    HelpScreen m -> H.view context m
    Playing game -> G.view context game
    LevelCompleted level -> L.view context level
    Victory m -> V.view context m
