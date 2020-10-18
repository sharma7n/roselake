module Weapon exposing
    ( Weapon
    , byId
    , generator
    )

import Random

type alias Weapon =
    { name : String
    , cost : Int
    , attack : Int
    }

byId : String -> Weapon
byId id =
    case id of
        "sword" ->
            { name = "Sword"
            , cost = 1
            , attack = 1
            }
        
        _ ->
            { name = "Null Weapon"
            , cost = 0
            , attack = 0
            }

generator : Random.Generator Weapon
generator =
    Random.weighted
        ( 0, byId "" )
        [ ( 1, byId "sword" )
        ]