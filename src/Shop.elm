module Shop exposing
    ( Shop
    , byId
    , generator
    )

import Random

import Item exposing (Item)
type alias Shop =
    { id : String
    , name : String
    , stock : List Item
    }

byId : String -> Shop
byId id =
    case id of
        "potionshop" ->
            { id = "potionshop"
            , name = "Potion Shop"
            , stock =
                [ Item.byId "potion"
                ]
            }
        
        _ ->
            { id = "null"
            , name = "Null Shop"
            , stock = []
            }

generator : Random.Generator Shop
generator =
    Random.list 10 Item.generator
        |> Random.andThen (\items ->
            let
                unique =
                    List.foldl (\item -> \l ->
                        if List.member item l then
                            l
                        else
                            item :: l
                    ) [] items
            in
            Random.constant <|
                { id = "dungeonshop"
                , name = "Dungeon Shop"
                , stock = unique
                }
        )