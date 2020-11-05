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

base : Essentia
base =
    { id = "null"
    , name = "Null Essentia"
    , actions = []
    , passives = []
    , attack = 0
    , defense = 0
    , magic = 0
    , agility = 0
    }

generator : Random.Generator Essentia
generator =
    Random.weighted
        ( 0, byId "" )
        [ ( 33, byId "green" )
        , ( 33, byId "blue" )
        , ( 33, byId "red" )
        ]

listStarting : List Essentia
listStarting =
    [ byId "green"
    , byId "blue"
    , byId "red"
    ]
byId : String -> Essentia
byId id =
    case id of
        "green" ->
            { base
                | id = "green"
                , name = "Green"
            }
        
        "red" ->
            { base
                | id = "red"
                , name = "Red"
            }
        
        "blue" ->
            { base
                | id = "blue"
                , name = "Blue"
            }
        
        _ ->
            base