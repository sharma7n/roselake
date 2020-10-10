module Action exposing
    ( Action
    , byId
    )

import Effect exposing (Effect)

type alias Action =
    { name : String
    , effects : List Effect
    }

byId : String -> Action
byId id =
    case id of
        "attack" ->
            { name = "Attack"
            , effects = []
            }
        
        _ ->
            { name = "Null Action"
            , effects = []
            }