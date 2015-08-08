module Input where

import Keyboard
import Time


type KeyboardInput = Up | Down | Left | Right | Esc | Enter


input: Signal KeyboardInput
input =
    let
        esc = Keyboard.isDown 27
            |> Signal.filter identity False
            |> Signal.map (always Esc)
        enter = Keyboard.isDown 13
            |> Signal.filter identity False
            |> Signal.map (always Enter)
    in
        Signal.mergeMany
            [ esc
            , enter
            , repeatableSignal 37 Left
            , repeatableSignal 38 Up
            , repeatableSignal 39 Right
            , repeatableSignal 40 Down
            ]

repeatableSignal: Keyboard.KeyCode -> KeyboardInput -> Signal KeyboardInput
repeatableSignal keycode input =
    let
        baseSignal =
            Keyboard.isDown keycode

        tick =
            Time.every (100 * Time.millisecond)

        countSignal =
            baseSignal
                |> Signal.foldp (\s (_, c) -> (s, c+1)) (False, 0)
    in
        countSignal
            |> Time.delay 333
            |> Signal.map2 (\(s1, c1) (s2, c2) -> s1 && s2 && c1 == c2) countSignal
            |> Signal.sampleOn tick
            |> Signal.merge baseSignal
            |> Signal.filter identity False
            |> Signal.map (always input)
