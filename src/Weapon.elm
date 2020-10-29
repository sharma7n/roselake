module Weapon exposing
    ( Weapon
    , byId
    , generator
    , listStarting
    )

import Random

import WeaponKind exposing (WeaponKind)

type alias Weapon =
    { id : String
    , name : String
    , kind : WeaponKind
    , cost : Int
    , attack : Int
    , magic : Int
    }

byId : String -> Weapon
byId id =
    case id of
        "training-axe" ->
            { id = "training-axe"
            , name = "Training Axe"
            , kind = WeaponKind.Axe
            , cost = 1
            , attack = 2
            , magic = 0
            }
        
        "training-bow" ->
            { id = "training-bow"
            , name = "Training Bow"
            , kind = WeaponKind.Bow
            , cost = 1
            , attack = 1
            , magic = 0
            }
        
        "training-claw" ->
            { id = "training-claw"
            , name = "Training Claw"
            , kind = WeaponKind.Claw
            , cost = 1
            , attack = 1
            , magic = 0
            }
        
        "training-staff" ->
            { id = "training-staff"
            , name = "Training Staff"
            , kind = WeaponKind.Staff
            , cost = 1
            , attack = 0
            , magic = 1
            }
        
        _ ->
            { id = "null"
            , name = "Null Weapon"
            , kind = WeaponKind.Axe
            , cost = 0
            , attack = 0
            , magic = 0
            }

generator : Random.Generator Weapon
generator =
    Random.weighted
        ( 0, byId "" )
        [ ( 1, byId "sword" )
        , ( 2, byId "sword2" )
        , ( 3, byId "sword3" )
        ]

listStarting : List Weapon
listStarting =
    [ byId "training-axe"
    , byId "training-bow"
    , byId "training-claw"
    , byId "training-staff"
    ]