module Monster exposing
    ( Monster
    , byId
    , generator
    )

import Random

type alias Monster =
    { name : String
    , experience : Int
    , hitPoints : Int
    , maxHitPoints : Int
    , attack : Int
    }

byId : String -> Monster
byId id =
    case id of
        "slime" ->
            { name = "Slime"
            , experience = 1
            , hitPoints = 3
            , maxHitPoints = 3
            , attack = 1
            }
        
        "gremlin" ->
            { name = "Gremlin"
            , experience = 1
            , hitPoints = 10
            , maxHitPoints = 10
            , attack = 1
            }
        
        _ ->
            { name = "Null Monster"
            , experience = 0
            , hitPoints = 0
            , maxHitPoints = 0
            , attack = 0
            }

generator : Random.Generator Monster
generator =
    Random.weighted
        ( 0, byId "" )
        [ ( 1, byId "attack" )
        ]