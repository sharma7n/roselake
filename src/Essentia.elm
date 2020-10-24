module Essentia exposing
    ( Essentia
    , byId
    )

import Action exposing (Action)
import Passive exposing (Passive)

type alias Essentia =
    { id : String
    , name : String
    , actions : List Action
    , passives : List Passive
    , attack : Int
    , defense : Int
    , magic : Int
    , agility : Int
    }

base : Essentia
base =
    { id = "null"
    , name = "Null Essentia"
    , actions = []
    , passives = []
    , attack = 0
    , defense = 0
    , magic = 0
    , agility = 0
    }

byId : String -> Essentia
byId id =
    case id of
        "green" ->
            { base
                | id = "green"
                , name = "Green"
            }
        
        "red" ->
            { base
                | id = "red"
                , name = "Red"
            }
        
        "blue" ->
            { base
                | id = "blue"
                , name = "Blue"
            }
        
        _ ->
            base