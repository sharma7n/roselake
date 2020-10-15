module Action exposing
    ( Action
    , byId
    , all
    )

import Context exposing (Context)
import Effect exposing (Effect)

type alias Action =
    { id : String
    , name : String
    , context : Context
    , learnCost : Int
    , magicPointCost : Int
    , effects : List Effect
    }

byId : String -> Action
byId id =
    case id of
        "attack" ->
            { id = "attack"
            , name = "Attack"
            , context = Context.Battle
            , learnCost = 0
            , magicPointCost = 0
            , effects = 
                [ Effect.ChangeMonsterHitPoints -1
                ]
            }
        
        "fireball" ->
            { id = "fireball"
            , name = "Fireball"
            , context = Context.Battle
            , learnCost = 1
            , magicPointCost = 1
            , effects =
                [ Effect.ChangeMonsterHitPoints -3
                ]
            }
        
        "heal" ->
            { id = "heal"
            , name = "Heal"
            , context = Context.Any
            , learnCost = 1
            , magicPointCost = 2
            , effects =
                [ Effect.ChangeHitPoints 1
                ]
            }
        
        _ ->
            { id = "null"
            , name = "Null Action"
            , context = Context.None
            , learnCost = 0
            , magicPointCost = 0
            , effects = []
            }

all : List Action
all =
    [ byId "attack"
    , byId "fireball"
    , byId "heal"
    ]