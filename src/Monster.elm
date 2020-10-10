module Monster exposing
    ( Monster
    , byId
    )

type alias Monster =
    { name : String
    , hitPoints : Int
    }

byId : String -> Monster
byId id =
    case id of
        "gremlin" ->
            { name = "Gremlin"
            , hitPoints = 5
            }
        
        _ ->
            { name = "Null Monster"
            , hitPoints = 0
            }