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
    if retries > 2 then
        Random.constant <| Action.byId "attack"
    else
        let
            selectedDistribution =
                battle.monster.behaviors
                    |> List.filter (\b ->
                        case b.condition of
                            Behavior.Always ->
                                True
                            
                            Behavior.BelowHitPointThreshold threshold ->
                                toFloat battle.monster.hitPoints <= threshold * toFloat battle.monster.maxHitPoints
                            
                            Behavior.RoundSchedule first length ->
                                Debug.log "modulus" (modBy (Debug.log "length" length) ((Debug.log "battle.round" battle.round) - (Debug.log "first" first))) == 0
                    )
                    |> List.foldl (\b -> \m ->
                        if b.priority >= Maybe.withDefault 0 (Maybe.map .priority m) then
                            Just b
                        else
                            m
                    ) Nothing
                    |> Maybe.map .actionDistribution
                    |> Maybe.withDefault (Distribution.new ( 100, Action.byId "nothing" ) [])

        in
        Distribution.random selectedDistribution
            |> Random.andThen (\action ->
                if action.magicPointCost <= battle.monster.magicPoints then
                    Random.constant action
                else
                    chooseMonsterActionInternal battle (retries + 1)
            )