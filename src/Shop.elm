module Shop exposing
    ( Shop
    , byId
    , generator
    )

import Random

import Object exposing (Object)

type alias Shop =
    { id : String
    , name : String
    , stock : List Object
    }

byId : String -> Shop
byId id =
    case id of
        "potionshop" ->
            { id = "potionshop"
            , name = "Potion Shop"
            , stock =
                [ Object.byId Object.ItemType "potion"
                ]
            }
        
        _ ->
            { id = "null"
            , name = "Null Shop"
            , stock = []
            }

generator : Random.Generator Shop
generator =
    Random.list 10 Object.generator
        |> Random.andThen (\objects ->
            let
                unique =
                    List.foldl (\object -> \l ->
                        if List.member object l then
                            l
                        else
                            object :: l
                    ) [] objects
            in
            Random.constant <|
                { id = "dungeonshop"
                , name = "Dungeon Shop"
                , stock = unique
                }
        )