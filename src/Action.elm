module Action exposing
    ( Action
    , byId
    )

import BattleEffect exposing (BattleEffect)

type alias Action =
    { name : String
    , effects : List BattleEffect
    }

byId : String -> Action
byId id =
    case id of
        "attack" ->
            { name = "Attack"
            , effects = 
                [ BattleEffect.ChangeMonsterHitPoints -1
                ]
            }
        
        _ ->
            { name = "Null Action"
            , effects = []
            }