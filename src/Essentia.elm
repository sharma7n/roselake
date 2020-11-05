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
        [ ( 1, byId "body" )
        , ( 1, byId "mind" )
        , ( 1, byId "nature" )
        , ( 1, byId "technology" )
        , ( 1, byId "spirit" )
        ]

listStarting : List Essentia
listStarting =
    [ byId "body"
    , byId "mind"
    , byId "nature"
    , byId "technology"
    , byId "spirit"
    ]

byId : String -> Essentia
byId =
    Util.getById all default

all : List Essentia
all =
    [ new "Body"
        [ Action.byId "tackle"
        , Action.byId "buff-attack"
        ]
        []
        (\e ->
            e
        )
    , new "Mind"
        []
        []
        (\e ->
            e
        )
    , new "Nature"
        []
        []
        (\e ->
            e
        )
    , new "Technology"
        []
        []
        (\e ->
            e
        )
    , new "Spirit"
        []
        []
        (\e ->
            e
        )
    ]