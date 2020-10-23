module Status exposing
    ( Status(..)
    , toString
    )

type Status
    = ModifyAttack
    | ModifyDefense
    | Curse
    | Poison

toString : Status -> String
toString s =
    case s of
        ModifyAttack ->
            "Attack"
        
        ModifyDefense ->
            "Defense"
        
        Curse ->
            "Curse"
        
        Poison ->
            "Poison"