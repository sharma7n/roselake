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
import Shop exposing (Shop)

type Scene
    = Empty
    | Trap
    | Battle
    | BattleMonsterLoadingIntent Monster
    | BattleMonster Monster Action
    | VictoryLoading Monster
    | Victory Monster Reward
    | Treasure
    | ReceiveTreasure Object
    | Event
    | RestArea
    | Rested
    | Shop
    | Shopping Shop
    | TrapDoor
    | LoadingGoal
    | Goal Reward

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
        
        VictoryLoading _ ->
            "Loading Victory..."
        
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
        
        Rested ->
            "Rested"
        
        Shop ->
            "Shop"
        
        Shopping shop ->
            "Shopping at " ++ shop.name
        
        TrapDoor ->
            "Trap Door"
        
        LoadingGoal ->
            "Loading goal..."
        
        Goal _ ->
            "Goal"