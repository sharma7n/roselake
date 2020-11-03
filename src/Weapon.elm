module Weapon exposing
    ( Weapon
    , byId
    , generator
    , listStarting
    )

import Random

import Action exposing (Action)
import Util

type alias Weapon =
    { id : String
    , name : String
    , cost : Int
    , actions : List Action
    , attack : Int
    , magic : Int
    }

new : String -> (Weapon -> Weapon) -> Weapon
new name f =
    { id = Util.kebabify name
    , name = name
    , cost = 0
    , actions = []
    , attack = 0
    , magic = 0
    }
        |> f

byId : String -> Weapon
byId =
    let
        default =
            new "Null Weapon" (\w -> w)
    in
    Util.getById all default

all : List Weapon
all =
    [ new "Training Axe"
        (\w ->
            { w
                | cost = 1
                , actions = 
                    [ Action.byId "axe-attack"
                    ]
                , attack = 2
            }
        )
    , new "Training Bow"
        (\w ->
            { w
                | cost = 1
                , actions = 
                    [ Action.byId "bow-attack"
                    ]
                , attack = 1
            }
        )
    , new "Training Claw"
        (\w ->
            { w
                | cost = 1
                , actions = 
                    [ Action.byId "claw-attack"
                    ]
                , attack = 1
            }
        )
    , new "Training Staff"
        (\w ->
            { w
                | cost = 1
                , actions = 
                    [ Action.byId "staff-attack"
                    ]
                , magic = 1
            }
        )
    ]

generator : Random.Generator Weapon
generator =
    Random.weighted
        ( 0, byId "" )
        [ ( 25, byId "training-axe" )
        , ( 25, byId "training-bow" )
        , ( 25, byId "training-claw" )
        , ( 25, byId "training-staff" )
        ]

listStarting : List Weapon
listStarting =
    [ byId "training-axe"
    , byId "training-bow"
    , byId "training-claw"
    , byId "training-staff"
    ]