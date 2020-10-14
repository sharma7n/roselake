module Inventory exposing
    ( Inventory
    , new
    , modify
    , toList
    )

import Dict exposing (Dict)

import Item exposing (Item)

type Inventory
    = Inventory (Dict String Int)

new : Inventory
new =
    Inventory Dict.empty

fromItems : List ( Item, Int ) -> Inventory
fromItems itemsAndQuantities =
    itemsAndQuantities
        |> List.map (\(item, qty) -> (item.id, qty))
        |> Dict.fromList
        |> Inventory

modify : Item -> Int -> Inventory -> Inventory
modify item delta (Inventory dict) =
    let
        newDict =
            Dict.update
                item.id
                (Maybe.map (\qty -> max 0 (qty + delta)))
                dict
    in
    Inventory newDict

toList : Inventory -> List ( Item, Int )
toList (Inventory dict) =
    dict
        |> Dict.toList
        |> List.map (\(k, v) -> (Item.byId k, v))