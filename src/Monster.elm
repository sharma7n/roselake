module Monster exposing
    ( Monster
    , byId
    )

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