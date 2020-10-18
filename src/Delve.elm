module Delve exposing
    ( Delve
    )

import Dungeon exposing (Dungeon)

type alias Delve =
    { dungeon : Dungeon
    , floor : Int
    }