module Update (update) where

import Model exposing (moveCharacter, Model)
import Input exposing (Input)


update: Input -> Model -> Model
update input model =
    case model.state of
        Model.Menu -> updateMenu input model
        Model.Playing -> updatePlaying input model
        Model.LevelCompleted -> updateCompleted input model
        Model.Victory -> model


updateMenu: Input -> Model -> Model
updateMenu input model = model


updateCompleted: Input -> Model -> Model
updateCompleted _ model = Model.moveToNextLevel model


updatePlaying: Input -> Model -> Model
updatePlaying input model =
    let
        newModel = case input of
            Input.KeyUp -> { model | current <- moveCharacter Model.Up model.current }
            Input.KeyDown -> { model | current <- moveCharacter Model.Down model.current }
            Input.KeyLeft -> { model | current <- moveCharacter Model.Left model.current }
            Input.KeyRight -> { model | current <- moveCharacter Model.Right model.current }
            Input.KeyEsc -> Model.reset model
    in
        if (Model.isCompleted newModel.current) then { newModel | state <- Model.LevelCompleted } else newModel
