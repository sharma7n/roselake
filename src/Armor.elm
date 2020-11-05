module Armor exposing
    ( Armor
    , byId
    , generator
    )

import Random

import Util

type alias Armor =
    { id : String
    , name : String
    , cost : Int
    , defense : Int
    }

default : Armor
default =
    { id = "null"
    , name = "Null Armor"
    , cost = 0
    , defense = 0
    }

new : String -> (Armor -> Armor) -> Armor
new name f =
    { id = Util.kebabify name
    , name = name
    , cost = 0
    , defense = 0
    }
        |> f

byId : String -> Armor
byId =
    Util.getById all default

generator : Random.Generator Armor
generator =
    Random.weighted
        ( 0, byId "null" )
        [ ( 100, byId "cotton-shirt" )
        ]

all : List Armor
all =
    [ new "Cotton Shirt"
        (\a ->
            { a
                | cost = 1
            }
        )
    ]