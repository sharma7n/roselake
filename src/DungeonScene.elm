module DungeonScene exposing
    ( Scene(..)
    , generator
    , toString
    )

import Random

import Action exposing (Action)
import Battle exposing (Battle)
import Monster exposing (Monster)
import Reward exposing (Reward)
import Object exposing (Object)
import Shop exposing (Shop)

type Scene
    = Empty
    | Trap
    | Battle
    | BattleMonsterLoadingIntent Battle
    | BattleMonster Battle Action
    | VictoryLoading Battle
    | Victory Battle Reward
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
    | Escaped
    | Discovery

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
        , Discovery
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
        
        BattleMonster battle _ ->
            "Battling: " ++ battle.monster.name
        
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
        
        Escaped ->
            "Escaped"
        
        Discovery ->
            "Discovery"