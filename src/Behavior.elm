module Behavior exposing
    ( Behavior
    , new
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

new : Int -> Condition -> Distribution Action -> Behavior
new priority condition actionDistribution =
    { priority = priority
    , condition = condition
    , actionDistribution = actionDistribution
    }

type Condition
    = Any
    | BelowHitPointThreshold Float
    | RoundSchedule Int Int

chooseAction : List Behavior -> Random.Generator Action
chooseAction s =
    Random.constant <|
        Action.byId ""