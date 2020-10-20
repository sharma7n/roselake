module Strategy exposing
    ( Strategy
    , chooseAction
    )

import Random

import Distribution exposing (Distribution)

import Action exposing (Action)

type alias Strategy =
    { priority : Int
    }

chooseAction : List Strategy -> Random.Generator Action
chooseAction s =
    Random.constant <|
        Action.byId ""