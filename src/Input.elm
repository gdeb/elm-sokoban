module Input where

import Keyboard


type Input = KeyUp | KeyDown | KeyLeft | KeyRight | KeyEsc


input: Signal Input
input =
    let
        makeSignal keycode input = Keyboard.isDown keycode
            |> Signal.filter identity False
            |> Signal.map (always input)

        esc = makeSignal 27 KeyEsc
        left = makeSignal 37 KeyLeft
        up = makeSignal 38 KeyUp
        right = makeSignal 39 KeyRight
        down = makeSignal 40 KeyDown
    in
        Signal.mergeMany [esc, left, up, right, down]
