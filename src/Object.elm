module Object exposing
    ( Object(..)
    , generator
    , toString
    )

import Random

import Armor exposing (Armor)
import Item exposing (Item)
import Weapon exposing (Weapon)

type Object
    = Item Item
    | Weapon Weapon
    | Armor Armor

type ObjectType
    = ItemType
    | WeaponType
    | ArmorType

objectTypeGenerator : Random.Generator ObjectType
objectTypeGenerator =
    Random.uniform
        ItemType
        [ WeaponType
        , ArmorType
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
                
                ArmorType ->
                    Random.map Armor Armor.generator
        )

toString : Object -> String
toString o =
    case o of
        Item i ->
            i.name
        
        Weapon w ->
            w.name
        
        Armor a ->
            a.name