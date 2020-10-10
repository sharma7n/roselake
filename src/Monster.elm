module Monster exposing
    ( Monster
    , byId
    )

type alias Monster =
    { name : String
    , hitPoints : Int
    , attack : Int
    }

byId : String -> Monster
byId id =
    case id of
        "gremlin" ->
            { name = "Gremlin"
            , hitPoints = 5
            , attack = 1
            }
        
        _ ->
            { name = "Null Monster"
            , hitPoints = 0
            , attack = 0
            }