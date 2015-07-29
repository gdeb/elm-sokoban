module Levels (levels) where

import Model exposing (Level, Tile)
import Dict

type LevelDescription = Empty | Wall | Floor | Goal | Box | Start

e = Empty
w = Wall
f = Floor
g = Goal
b = Box
s = Start

-- list of levels used in the game
levels: List Level
levels =
    [ makeLevel  -- level one
        [ [e, e, e, e, w, w, w, w, w]
        , [e, e, e, e, w, f, f, f, w]
        , [e, e, e, e, w, b, f, f, w]
        , [e, e, w, w, w, f, f, b, w, w]
        , [e, e, w, f, f, b, f, b, f, w]
        , [w, w, w, f, w, f, w, w, f, w, e, e, e, w, w, w, w, w, w]
        , [w, f, f, f, w, f, w, w, f, w, w, w, w, w, f, f, g, g, w]
        , [w, f, b, f, f, b, f, f, f, f, f, f, f, f, f, f, g, g, w]
        , [w, w, w, w, w, f, w, w, w, f, w, s, w, w, f, f, g, g, w]
        , [e, e, e, e, w, f, f, f, f, f, w, w, w, w, w, w, w, w, w]
        , [e, e, e, e, w, w, w, w, w, w, w]
        ]
    , makeLevel  -- level two
        [ [w, w, w, w, w, w, w, w, w, w, w, w]
        , [w, g, g, f, f, w, f, f, f, f, f, w, w, w]
        , [w, g, g, f, f, w, f, b, f, f, b, f, f, w]
        , [w, g, g, f, f, w, b, w, w, w, w, f, f, w]
        , [w, g, g, f, f, f, f, s, f, w, w, f, f, w]
        , [w, g, g, f, f, w, f, w, f, f, b, f, w, w]
        , [w, w, w, w, w, w, f, w, w, b, f, b, f, w]
        , [e, e, w, f, b, f, f, b, f, b, f, b, f, w]
        , [e, e, w, f, f, f, f, w, f, f, f, f, f, w]
        , [e, e, w, w, w, w, w, w, w, w, w, w, w, w]
        ]
    , makeLevel  -- level three
        [ [e, e, e, e, e, e, w, w, w, w, w]
        , [w, w, w, w, w, w, w, f, s, f, w]
        , [w, f, b, f, b, f, b, b, b, f, w]
        , [w, g, g, g, g, g, g, g, g, g, w]
        , [w, b, f, b, f, b, f, b, f, f, w]
        , [w, f, w, f, w, w, w, w, w, w, w]
        , [w, f, f, f, w]
        , [w, w, w, w, w]
        ]
    ]


tileKind : LevelDescription -> Tile
tileKind descr = case descr of
    Wall -> Model.Wall
    Goal -> Model.Goal
    otherwise -> Model.Floor


trivialLevel: Level
trivialLevel =
    { height = 0
    , width = 0
    , map = Dict.empty
    , boxes = []
    , character = (0,0)
    , moveCounter = 0
    , pushCounter = 0
    }


-- helper function to load a level from a simple description
makeLevel: List (List LevelDescription) -> Level
makeLevel description =
    let
        indexedTiles: List (Int, Int, LevelDescription)
        indexedTiles =
            description
                |> List.indexedMap (\i l -> List.indexedMap (\j t -> (j, i, t)) l)
                |> List.concat
                |> List.filter (\(_, _, ld) -> (ld /= Empty))

        reducer: (Int, Int, LevelDescription) -> Level -> Level
        reducer (x, y, descr) level =
            { level | width <- max (x + 1) level.width
                    , height <- max (y + 1) level.height
                    , map <- Dict.insert (x, y) (tileKind descr) level.map
                    , boxes <- if (descr == Box) then (x,y) :: level.boxes else level.boxes
                    , character <- if (descr == Start) then (x,y) else level.character }
    in
        List.foldl reducer trivialLevel indexedTiles
