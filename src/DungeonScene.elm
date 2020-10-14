module DungeonScene exposing
    ( Scene(..)
    , generator
    , toString
    )

import Random

import Monster exposing (Monster)
import Reward exposing (Reward)

type Scene
    = Empty
    | Trap
    | Battle
    | BattleMonster Monster
    | Victory Monster Reward
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
        
        BattleMonster monster ->
            "Battling: " ++ monster.name
        
        Victory _ _ ->
            "Victory"
        
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