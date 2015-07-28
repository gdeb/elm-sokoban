module Update where

import Model
import Input exposing (Input)


update: Input -> Model.Model -> Model.Model
update input model =
    case model.state of
        Model.Menu -> updateMenu input model
        Model.Playing -> updatePlaying input model
        Model.LevelCompleted -> updateCompleted input model
        Model.Victory -> model


updateMenu: Input -> Model.Model -> Model.Model
updateMenu input model = model

updateCompleted: Input -> Model.Model -> Model.Model
updateCompleted input model = case input of
    Input.None -> model
    otherwise -> Model.moveToNextLevel model


updatePlaying: Input -> Model.Model -> Model.Model
updatePlaying input model =
    let
        newModel =
            case input of
                Input.KeyUp -> { model | current <- Model.moveCharacter Model.Up model.current }
                Input.KeyDown -> { model | current <- Model.moveCharacter Model.Down model.current }
                Input.KeyLeft -> { model | current <- Model.moveCharacter Model.Left model.current }
                Input.KeyRight -> { model | current <- Model.moveCharacter Model.Right model.current }
                Input.KeyEsc -> Model.reset model
                otherwise -> model
    in
        if (Model.isCompleted newModel.current) then { newModel | state <- Model.LevelCompleted } else newModel
