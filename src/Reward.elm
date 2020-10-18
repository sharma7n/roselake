module Reward exposing
    ( Reward
    )

import Item exposing (Item)
import Weapon exposing (Weapon)

type alias Reward =
    { experience : Int
    , gold : Int
    , abilityPoints : Int
    , items : List ( Item, Int )
    , weapons : List ( Weapon, Int )
    }