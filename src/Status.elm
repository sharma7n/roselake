module Status exposing
    ( Status(..)
    , toString
    )

type Status
    = ModifyAttack
    | ModifyDefense
    | ModifyMagic
    | ModifyBlock
    | Curse
    | Poison
    | Burn

toString : Status -> String
toString s =
    case s of
        ModifyAttack ->
            "Attack"
        
        ModifyDefense ->
            "Defense"
        
        ModifyMagic ->
            "Magic"
        
        ModifyBlock ->
            "Block"
        
        Curse ->
            "Curse"
        
        Poison ->
            "Poison"
        
        Burn ->
            "Burn"