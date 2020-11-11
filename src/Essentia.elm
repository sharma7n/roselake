module Essentia exposing
    ( Essentia
    , byId
    , generator
    , listStarting
    )

import Random

import Action exposing (Action)
import Passive exposing (Passive)

import Util

type alias Essentia =
    { id : String
    , name : String
    , actions : List Action
    , passives : List Passive
    , attack : Int
    , defense : Int
    , magic : Int
    , agility : Int
    }

default : Essentia
default =
    { id = "null"
    , name = "Null Essentia"
    , actions = []
    , passives = []
    , attack = 0
    , defense = 0
    , magic = 0
    , agility = 0
    }

new : String -> List Action -> List Passive -> (Essentia -> Essentia) -> Essentia
new name actions passives f =
    { default
        | id = Util.kebabify name
        , name = name
        , actions = actions
        , passives = passives
    }
        |> f

generator : Random.Generator Essentia
generator =
    Random.weighted
        ( 0, byId "" )
        [ ( 1, byId "tough" )
        , ( 1, byId "pupil" )
        , ( 1, byId "wanderer" )
        ]

listStarting : List Essentia
listStarting =
    [ byId "tough"
    , byId "pupil"
    , byId "wanderer"
    ]

byId : String -> Essentia
byId =
    Util.getById all default

all : List Essentia
all =
    [ new "Tough"
        [ Action.byId "tackle"
        , Action.byId "defend"
        , Action.byId "focus-attack"
        ]
        [ Passive.byId "p-counter:-defend"
        , Passive.byId "p-counter:-tackle"
        ]
        (\e ->
            e
        )
    , new "Pupil"
        [ Action.byId "half-fire"
        , Action.byId "half-ice"
        ]
        []
        (\e ->
            e
        )
    , new "Wanderer"
        [ Action.byId "flee"
        ]
        []
        (\e ->
            e
        )
    ]