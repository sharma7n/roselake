module Item exposing
    ( Item
    , byId
    , generator
    )

import Random

import Context exposing (Context)
import Effect exposing (Effect)

type alias Item =
    { id : String
    , name : String
    , cost : Int
    , context : Context
    , effects : List Effect
    }

byId : String -> Item
byId s =
    case s of
        "potion" ->
            { id = "potion"
            , name = "Potion"
            , cost = 2
            , context = Context.Any
            , effects =
                [ Effect.ChangeHitPoints 1
                ]
            }
        
        _ ->
            { id = "null"
            , name = "Null Item"
            , cost = 0
            , context = Context.None
            , effects = []
            }

generator : Random.Generator Item
generator =
    Random.weighted
        ( 0, byId "" )
        [ ( 1, byId "potion" )
        ]