module Context where

import Window

type alias Context =
    { width: Int
    , height: Int
    }


context: Signal Context
context =
    Signal.map (\(w, h) -> {width = w, height = h}) Window.dimensions
