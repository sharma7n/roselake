module BossScene exposing
    ( BossScene(..)
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

type BossScene
    = Empty
    | BattleBoss
    | BattleBossLoadingIntent Battle
    | BattleBossOngoing Battle Action
    --| Trap
    --| Battle
    --| BattleMonsterLoadingIntent Battle
    --| BattleMonster Battle Action
    | VictoryLoading Battle
    | Victory Battle Reward
    --| Treasure
    --| ReceiveTreasure Object
    --| Event
    --| RestArea
    --| Rested
    --| Shop
    --| Shopping Shop
    --| TrapDoor
    --| LoadingGoal
    ---| Goal Reward
    | Escaped

generator : Random.Generator BossScene
generator =
    Random.uniform
        Empty
        []

toString : BossScene -> String
toString s =
    case s of
        Empty ->
            "Empty"
        
        BattleBoss ->
            "Battle Boss"
        
        BattleBossLoadingIntent _ ->
            "Battle Boss"
        
        BattleBossOngoing _ _ ->
            "Battle Boss"
        
        Escaped ->
            "Escaped"
        
        VictoryLoading _ ->
            "Victory Loading"
        
        Victory _ _ ->
            "Victory"