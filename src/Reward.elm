module Reward exposing
    ( Reward
    )

import Item exposing (Item)

type alias Reward =
    { experience : Int
    , gold : Int
    , items : List ( Item, Int )
    }