module Object exposing
    ( Object(..)
    , generator
    , toString
    )

import Random

import Item exposing (Item)

type Object
    = Item Item

generator : Random.Generator Object
generator =
    Random.map Item Item.generator

toString : Object -> String
toString o =
    case o of
        Item i ->
            i.name