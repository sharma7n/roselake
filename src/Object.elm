module Object exposing
    ( Object(..)
    , generator
    , toString
    )

import Random

import Item exposing (Item)
import Weapon exposing (Weapon)

type Object
    = Item Item
    | Weapon Weapon

type ObjectType
    = ItemType
    | WeaponType

objectTypeGenerator : Random.Generator ObjectType
objectTypeGenerator =
    Random.uniform
        ItemType
        [ WeaponType
        ]

generator : Random.Generator Object
generator =
    objectTypeGenerator
        |> Random.andThen (\objectType ->
            case objectType of
                ItemType ->
                    Random.map Item Item.generator
                
                WeaponType ->
                    Random.map Weapon Weapon.generator
        )

toString : Object -> String
toString o =
    case o of
        Item i ->
            i.name
        
        Weapon w ->
            w.name