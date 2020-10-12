module DungeonScene exposing
    ( Scene(..)
    , generator
    , toString
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

toString : Scene -> String
toString s =
    case s of
        Empty ->
            "Empty"
        
        Trap ->
            "Trap"
        
        Battle ->
            "Battle"
        
        Treasure ->
            "Treasure"
        
        Event ->
            "Event"
        
        RestArea ->
            "Rest Area"
        
        Shop ->
            "Shop"
        
        TrapDoor ->
            "Trap Door"
        
        Goal ->
            "Goal"