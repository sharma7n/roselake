module Inventory exposing
    ( Inventory
    , new
    , modifyItemQuantity
    , modifyWeaponQuantity
    , listItems
    , listWeapons
    )

import Dict exposing (Dict)

import Item exposing (Item)
import Weapon exposing (Weapon)

type Inventory
    = Inventory Data

type alias Data =
    { items : Dict String Int
    , weapons : Dict String Int
    }

type Slot
    = ItemSlot
    | WeaponSlot

new : Inventory
new =
    Inventory <|
        { items = Dict.empty
        , weapons = Dict.empty
        }

modifyQuantity : Slot -> { a | id : String } -> Int -> Inventory -> Inventory
modifyQuantity slot object delta (Inventory data) =
    let
        modifyCounter counter =
            let
                qty =
                    counter
                        |> Dict.get object.id
                        |> Maybe.withDefault 0
                
                newQty =
                    max 0 (qty + delta)
            in
            counter
                |> Dict.insert object.id newQty
        
        newData =
            case slot of
                ItemSlot ->
                    { data | items = modifyCounter data.items }

                WeaponSlot ->
                    { data | weapons = modifyCounter data.weapons }
    in
    Inventory newData

modifyItemQuantity : Item -> Int -> Inventory -> Inventory
modifyItemQuantity =
    modifyQuantity ItemSlot

modifyWeaponQuantity : Weapon -> Int -> Inventory -> Inventory
modifyWeaponQuantity =
    modifyQuantity WeaponSlot

listItems : Inventory -> List ( Item, Int )
listItems (Inventory data) =
    data.items
        |> Dict.toList
        |> List.map (\(id, q) -> (Item.byId id, q))

listWeapons : Inventory -> List ( Weapon, Int )
listWeapons (Inventory data) =
    data.weapons
        |> Dict.toList
        |> List.map (\(id, q) -> (Weapon.byId id, q))