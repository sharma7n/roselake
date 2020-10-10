module Action exposing
    ( Action
    , byId
    )

type alias Action =
    { name : String
    }

byId : String -> Action
byId id =
    case id of
        "attack" ->
            { name = "Attack"
            }
        
        _ ->
            { name = "Null Action"
            }