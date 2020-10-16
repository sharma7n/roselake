module Action exposing
    ( Action
    , byId
    , learnable
    )

import Context exposing (Context)
import Effect exposing (Effect)
import Formula exposing (Formula)
import Target exposing (Target)

type alias Action =
    { id : String
    , name : String
    , context : Context
    , learnCost : Int
    , magicPointCost : Int
    , subs : List Sub
    }

type alias Sub =
    { target : Target
    , effects : List Effect
    }

byId : String -> Action
byId id =
    case id of
        "nothing" ->
            { id = "nothing"
            , name = "Nothing"
            , context = Context.Any
            , learnCost = 0
            , magicPointCost = 0
            , subs = []
            }
        
        "attack" ->
            { id = "attack"
            , name = "Attack"
            , context = Context.Battle
            , learnCost = 0
            , magicPointCost = 0
            , subs =
                [ { target = Target.Enemy
                  , effects =
                    [ Effect.BattleEnemy Formula.Attack
                    ]
                  }
                ]
            }
        
        "fireball" ->
            { id = "fireball"
            , name = "Fireball"
            , context = Context.Battle
            , learnCost = 1
            , magicPointCost = 1
            , subs =
                [ { target = Target.Enemy
                  , effects =
                    [ Effect.BattleEnemy Formula.Fireball
                    ]
                  }
                ]
            }
        
        "heal" ->
            { id = "heal"
            , name = "Heal"
            , context = Context.Any
            , learnCost = 1
            , magicPointCost = 2
            , subs =
                [ { target = Target.Self
                  , effects =
                    [ Effect.BattleSelf Formula.Heal
                    ]
                  }
                ]
            }
        
        _ ->
            { id = "null"
            , name = "Null Action"
            , context = Context.None
            , learnCost = 0
            , magicPointCost = 0
            , subs = []
            }

learnable : List Action
learnable =
    [ byId "fireball"
    , byId "heal"
    ]