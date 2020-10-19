module Armor exposing
    ( Armor
    , byId
    , generator
    )

import Random

type alias Armor =
    { id : String
    , name : String
    , cost : Int
    , defense : Int
    }

byId : String -> Armor
byId id =
    case id of
        "shirt" ->
            { id = "shirt"
            , name = "Shirt"
            , cost = 10
            , defense = 0
            }
        
        "mail" ->
            { id = "mail"
            , name = "Mail"
            , cost = 20
            , defense = 1
            }
        
        "plate" ->
            { id = "plate"
            , name = "Plate"
            , cost = 60
            , defense = 2
            }
        
        _ ->
            { id = "null"
            , name = "Null Armor"
            , cost = 0
            , defense = 0
            }

generator : Random.Generator Armor
generator =
    Random.weighted
        ( 0, byId "null" )
        [ ( 3, byId "shirt" )
        , ( 2, byId "mail" )
        , ( 1, byId "plate" )
        ]