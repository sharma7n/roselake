module Status exposing
    ( Status(..)
    , toString
    )

type Status
    = ModifyAttack
    | ModifyDefense
    | ModifyBlock
    | Curse
    | Poison

toString : Status -> String
toString s =
    case s of
        ModifyAttack ->
            "Attack"
        
        ModifyDefense ->
            "Defense"
        
        ModifyBlock ->
            "Block"
        
        Curse ->
            "Curse"
        
        Poison ->
            "Poison"