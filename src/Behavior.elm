module Behavior exposing
    ( Behavior
    , Condition(..)
    , chooseAction
    )

import Random

import Distribution exposing (Distribution)

import Action exposing (Action)

type alias Behavior =
    { priority : Int
    , condition : Condition
    , actionDistribution : Distribution Action
    }

type Condition
    = Any
    | BelowHitPointThreshold Float
    | RoundModulo Int

chooseAction : List Behavior -> Random.Generator Action
chooseAction s =
    Random.constant <|
        Action.byId ""