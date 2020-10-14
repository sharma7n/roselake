module Shop exposing
    ( Shop
    , byId
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