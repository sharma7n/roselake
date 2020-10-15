module Action exposing
    ( Action
    , byId
    )

import Context exposing (Context)
import Effect exposing (Effect)

type alias Action =
    { name : String
    , context : Context
    , effects : List Effect
    }

byId : String -> Action
byId id =
    case id of
        "attack" ->
            { name = "Attack"
            , context = Context.Battle
            , effects = 
                [ Effect.ChangeMonsterHitPoints -1
                ]
            }
        
        "fireball" ->
            { name = "Fireball"
            , context = Context.Battle
            , effects =
                [ Effect.ChangeMagicPoints -1
                , Effect.ChangeMonsterHitPoints -3
                ]
            }
        
        _ ->
            { name = "Null Action"
            , context = Context.None
            , effects = []
            }