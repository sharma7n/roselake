module Status exposing
    ( Status
    , id
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