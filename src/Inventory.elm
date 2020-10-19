module Inventory exposing
    ( Inventory
    , new
    , modifyItemQuantity
    , modifyWeaponQuantity
    , modifyArmorQuantity
    , listItems
    , listWeapons
    , listArmors
    )

import Dict exposing (Dict)

import Armor exposing (Armor)
import Item exposing (Item)
import Weapon exposing (Weapon)

type Inventory
    = Inventory Data

type alias Data =
    { items : Dict String Int
    , weapons : Dict String Int
    , armors : Dict String Int
    }

type Slot
    = ItemSlot
    | WeaponSlot
    | ArmorSlot

new : Inventory
new =
    Inventory <|
        { items = Dict.empty
        , weapons = Dict.empty
        , armors = Dict.empty
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
                
                ArmorSlot ->
                    { data | armors = modifyCounter data.armors }
    
    in
    Inventory newData

modifyItemQuantity : Item -> Int -> Inventory -> Inventory
modifyItemQuantity =
    modifyQuantity ItemSlot

modifyWeaponQuantity : Weapon -> Int -> Inventory -> Inventory
modifyWeaponQuantity =
    modifyQuantity WeaponSlot

modifyArmorQuantity : Armor -> Int -> Inventory -> Inventory
modifyArmorQuantity =
    modifyQuantity ArmorSlot

listItems : Inventory -> List ( Item, Int )
listItems (Inventory data) =
    data.items
        |> Dict.toList
        |> List.map (\(id, q) -> (Item.byId id, q))
        |> List.filter (\(_, q) -> q > 0)

listWeapons : Inventory -> List ( Weapon, Int )
listWeapons (Inventory data) =
    data.weapons
        |> Dict.toList
        |> List.map (\(id, q) -> (Weapon.byId id, q))
        |> List.filter (\(_, q) -> q > 0)

listArmors : Inventory -> List ( Armor, Int )
listArmors (Inventory data) =
    data.armors
        |> Dict.toList
        |> List.map (\(id, q) -> (Armor.byId id, q))
        |> List.filter (\(_, q) -> q > 0)