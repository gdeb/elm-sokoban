module Input where

import Keyboard
import Time


type Input = KeyUp | KeyDown | KeyLeft | KeyRight | KeyEsc


input: Signal Input
input =
    let
        esc = Keyboard.isDown 27
            |> Signal.filter identity False
            |> Signal.map (always KeyEsc)
    in
        Signal.mergeMany
            [ esc
            , repeatableSignal 37 KeyLeft
            , repeatableSignal 38 KeyUp
            , repeatableSignal 39 KeyRight
            , repeatableSignal 40 KeyDown
            ]

repeatableSignal: Keyboard.KeyCode -> Input -> Signal Input
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
