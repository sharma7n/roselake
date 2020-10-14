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

modify : Item -> Int -> Inventory -> Inventory
modify item delta (Inventory dict) =
    let
        qty =
            dict
                |> Dict.get item.id
                |> Maybe.withDefault 0
        
        newQty =
            max 0 (qty + delta)
        
        newDict =
            dict
                |> Dict.insert item.id newQty
    in
    Inventory newDict

toList : Inventory -> List ( Item, Int )
toList (Inventory dict) =
    dict
        |> Dict.toList
        |> List.map (\(k, v) -> (Item.byId k, v))