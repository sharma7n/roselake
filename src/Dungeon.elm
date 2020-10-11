module Dungeon exposing
    ( Dungeon
    , byId
    )

type alias Dungeon =
    { name : String
    , depth : Int
    }

byId : String -> Dungeon
byId id =
    case id of
        "beginnerscave" ->
            { name = "Beginner's Cave"
            , depth = 10
            }
        
        _ ->
            { name = "Null Dungeon"
            , depth = 0
            }