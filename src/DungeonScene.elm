module DungeonScene exposing
    ( Scene(..)
    , generator
    )

import Random

type Scene
    = Empty
    | Trap
    | Battle
    | Treasure
    | Event
    | RestArea
    | Shop
    | TrapDoor
    | Goal

generator : Random.Generator Scene
generator =
    Random.uniform
        Empty
        [ Trap
        , Battle
        , Treasure
        , Event
        , RestArea
        , Shop
        , TrapDoor
        ]