module DungeonScene exposing
    ( Scene(..)
    , generator
    , toString
    )

import Random

import Action exposing (Action)
import Monster exposing (Monster)
import Reward exposing (Reward)
import Object exposing (Object)

type Scene
    = Empty
    | Trap
    | Battle
    | BattleMonsterLoadingIntent Monster
    | BattleMonster Monster Action
    | Victory Monster Reward
    | Treasure
    | ReceiveTreasure Object
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
        
        BattleMonsterLoadingIntent _ ->
            "Loading..."
        
        BattleMonster monster _ ->
            "Battling: " ++ monster.name
        
        Victory _ _ ->
            "Victory"
        
        Treasure ->
            "Treasure"
        
        ReceiveTreasure o ->
            "Got: " ++ Object.toString o
        
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