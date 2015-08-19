module Screens.Game where

import Color
import Dict
import Graphics.Element exposing (..)
import Text

import Context exposing (Context)
import Input exposing (KeyboardInput)
import Level
import Images.Title exposing (title)

-- model
type alias Model =
    { current: Level.Model
    , initial: Level.Model
    , levelNumber: Int
    }


-- update
type Request = LevelCompleted

update: KeyboardInput -> Model -> (Model, Maybe Request)
update input model =
    let
        isCompleted level =
            List.all (\p -> isGoal p level) level.boxes

        isGoal position level =
            Dict.get position level.map == Just Level.Goal

        newModel = case input of
            Input.Up -> { model | current <- Level.update Level.MoveUp model.current }
            Input.Down -> { model | current <- Level.update Level.MoveDown model.current }
            Input.Left -> { model | current <- Level.update Level.MoveLeft model.current }
            Input.Right -> { model | current <- Level.update Level.MoveRight model.current }
            Input.Esc -> { model | current <- model.initial }
            otherwise -> model

        request = if (isCompleted newModel.current) then Just LevelCompleted else Nothing
    in
        (newModel, request)


-- view
view: Context -> Model -> Element
view context model =
    let
        makeText str =
            Text.fromString str |> Text.height 20 |> Text.color Color.charcoal |> centered

        helpMessage =
            Text.fromString "arrows: move character, escape: reset level  "
                |> Text.height 15
                |> Text.color Color.charcoal
                |> centered
                |> container context.width 20 midRight

        currentLevel =
            Level.view model.current
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
            [ title context.width
            , helpMessage
            , currentLevel
            , statusBar
            ]
