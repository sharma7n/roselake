module Object exposing
    ( Object(..)
    , ObjectType(..)
    , byId
    , generator
    , name
    , cost
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

fold : (Item -> a) -> (Weapon -> a) -> (Armor -> a) -> Object -> a
fold fromItem fromWeapon fromArmor o =
    case o of
        Item i ->
            fromItem i
        
        Weapon w ->
            fromWeapon w
        
        Armor a ->
            fromArmor a

new : (a -> Item) -> (a -> Weapon) -> (a -> Armor) -> ObjectType -> a -> Object
new toItem toWeapon toArmor objectType x =
    case objectType of
        ItemType ->
            Item <| toItem x
        
        WeaponType ->
            Weapon <| toWeapon x
        
        ArmorType ->
            Armor <| toArmor x

byId : ObjectType -> String -> Object
byId =
    new Item.byId Weapon.byId Armor.byId

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

name : Object -> String
name =
    fold .name .name .name

cost : Object -> Int
cost =
    fold .cost .cost .cost