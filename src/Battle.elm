module Battle exposing
    ( Battle
    , completeRound
    , chooseMonsterAction
    )

import Random

import Distribution exposing (Distribution)
import NonEmptyList exposing (NonEmptyList)
import Util

import Action exposing (Action)
import Battler exposing (Battler)
import Behavior exposing (Behavior)
import Monster exposing (Monster)

type alias Battle =
    { round : Int
    , monster : Monster
    }

completeRound : Battle -> Battle
completeRound b =
    { round = b.round + 1
    , monster =
        Battler.completeRound b.monster
    }

chooseMonsterAction : Battle -> Random.Generator Action
chooseMonsterAction battle =
    chooseMonsterActionInternal battle 0

chooseMonsterActionInternal : Battle -> Int -> Random.Generator Action
chooseMonsterActionInternal battle retries =
    case battle.monster.behavior of
        Behavior.None ->
            Random.constant <| Action.byId "nothing"
        
        Behavior.Slime ->
            Random.weighted
                ( 50, Action.byId "nothing" )
                [ ( 50, Action.byId "attack" )
                ]