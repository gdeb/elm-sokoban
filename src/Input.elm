module Input where

import Keyboard


type Input = KeyUp | KeyDown | KeyLeft | KeyRight | KeyEsc | None


input: Signal Input
input =
    let
        escSignal = Signal.map (\pressed -> if pressed then KeyEsc else None) (Keyboard.isDown 27)
        getInput {x, y} =
            if | y < 0 -> KeyDown
               | y > 0 -> KeyUp
               | x < 0 -> KeyLeft
               | x > 0 -> KeyRight
               | otherwise -> None
    in
        Signal.merge
            (Signal.map getInput Keyboard.arrows)
            escSignal
