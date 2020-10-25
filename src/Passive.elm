module Passive exposing
    ( Passive
    , byId
    )

import Context exposing (Context)
import PassiveFormula exposing (PassiveFormula)

type alias Passive =
    { id : String
    , name : String
    , context : Context
    , learnCost : Int
    , effects : List PassiveFormula
    }

base : Passive
base =
    { id = "null"
    , name = "Null Passive"
    , context = Context.None
    , learnCost = 0
    , effects = []
    }

byId : String -> Passive
byId id =
    case id of
        "counter" ->
            { base
                | id = "counter"
                , name = "Counter"
                , context = Context.Battle
                , learnCost = 30
                , effects =
                    [ PassiveFormula.Counter
                    ]
            }
        
        _ ->
            base