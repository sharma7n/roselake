module Passive exposing
    ( Passive
    , byId
    )

import Context exposing (Context)
import PassiveFormula exposing (PassiveFormula)

import Util

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

new : String -> (Passive -> Passive) -> Passive
new name f =
    { id = Util.kebabify name
    , name = name
    , context = Context.None
    , learnCost = 0
    , effects = []
    }
        |> f

byId : String -> Passive
byId =
    Util.getById all base

all : List Passive
all =
    [ new "P-Counter: Defend"
        (\p ->
            { p
                | context = Context.Battle
                , learnCost = 20
                , effects =
                    [ PassiveFormula.PCounterDefend
                    ]
            }
        )
    ]