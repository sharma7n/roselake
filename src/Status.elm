module Status exposing
    ( Status(..)
    , id
    , toString
    )

type Status
    = ModifyAttack
    | ModifyDefense
    | Curse

id : Status -> String
id s =
    case s of
        ModifyAttack ->
            "modifyattack"
        
        ModifyDefense ->
            "modifydefense"
        
        Curse ->
            "curse"

toString : Status -> String
toString s =
    case s of
        ModifyAttack ->
            "Attack"
        
        ModifyDefense ->
            "Defense"
        
        Curse ->
            "Curse"