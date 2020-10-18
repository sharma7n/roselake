module Weapon exposing
    ( Weapon
    , byId
    , generator
    )

import Random

type alias Weapon =
    { id : String
    , name : String
    , cost : Int
    , attack : Int
    }

byId : String -> Weapon
byId id =
    case id of
        "sword" ->
            { id = "sword"
            , name = "Sword"
            , cost = 1
            , attack = 1
            }
        
        "sword2" ->
            { id = "sword2"
            , name = "Sword 2"
            , cost = 2
            , attack = 2
            }
        
        "sword3" ->
            { id = "sword3"
            , name = "Sword 3"
            , cost = 3
            , attack = 3
            }
        
        _ ->
            { id = "null"
            , name = "Null Weapon"
            , cost = 0
            , attack = 0
            }

generator : Random.Generator Weapon
generator =
    Random.weighted
        ( 0, byId "" )
        [ ( 1, byId "sword" )
        , ( 2, byId "sword2" )
        , ( 3, byId "sword3" )
        ]