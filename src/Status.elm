module Status exposing
    ( Status(..)
    , id
    , toString
    )

type Status
    = ModifyAttack
    | ModifyDefense

id : Status -> String
id s =
    case s of
        ModifyAttack ->
            "modifyattack"
        
        ModifyDefense ->
            "modifydefense"

toString : Status -> String
toString s =
    case s of
        ModifyAttack ->
            "Attack"
        
        ModifyDefense ->
            "Defense"