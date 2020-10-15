module Battler exposing
    ( Battler
    , applyEffects
    )

import Util

import Effect exposing (Effect)

type alias Battler a =
    { a
        | hitPoints : Int
        , maxHitPoints : Int
        , magicPoints : Int
        , maxMagicPoints : Int
        , attack : Int
        , agility : Int
    }

applyEffect : Effect -> Battler a -> Battler a
applyEffect effect b =
    case effect of
        Effect.ChangeHitPoints d ->
            { b | hitPoints = Util.boundedBy 0 b.maxHitPoints (b.hitPoints + d ) }
        
        Effect.ChangeMagicPoints d ->
            { b | magicPoints = Util.boundedBy 0 b.maxMagicPoints (b.magicPoints + d ) }
        
        _ ->
            b

applyEffects : List Effect -> Battler a -> Battler a
applyEffects effects b =
    List.foldl applyEffect b effects