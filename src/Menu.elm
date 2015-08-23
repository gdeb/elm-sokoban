module Menu where

import Color
import Graphics.Element exposing (flow, down, Element, centered)
import Graphics.Collage exposing (..)
import Text
-- 
-- import Context exposing (Context)
-- import Input exposing (KeyboardInput)
-- 
-- import Images.Title exposing (title)
import Images.Box exposing (box)

-- menu 
    
type alias Menu a = List (String, a)

        
-- model
type alias Model = Int


-- update
type Action = Up | Down | Select


update: Menu a -> Action -> Model -> (Model, Maybe a)
update menu input model =
    let
        dec s = (s - 1) % (List.length menu)
        inc s = (s + 1) % (List.length menu)
    in
        case input of
            Down -> (inc model, Nothing)
            Up -> (dec model, Nothing)
            Select ->
                case List.head (List.drop model menu) of
                    Just (_, request) -> (model, Just request)
                    Nothing -> (model, Nothing)
                    

-- view
view: Menu a -> Model -> Element
view menu index =
    let 
        decorate i (menu', _) = renderMenuLine menu' (i == index)
    in 
        collage 300 200
            [ rect 300 200 |> filled Color.lightGray
            , menu |> List.indexedMap decorate |> flow down |> toForm
            ]


renderMenuLine: String -> Bool -> Element
renderMenuLine text isActive =
    let
        menuText =
            Text.fromString text
                |> Text.height 20
                |> Text.color Color.charcoal
                |> centered

        decoration =
            if isActive then
                [ move (-80, 0) box
                , move (80, 0) box
                ]
            else
                []

        forms =
            List.append decoration [toForm menuText]
    in
        collage 300 50 forms
