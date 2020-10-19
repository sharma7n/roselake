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
    , actionPointCost : Int
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
            , actionPointCost = 0
            , magicPointCost = 0
            , subs = []
            }
        
        "attack" ->
            { id = "attack"
            , name = "Attack"
            , context = Context.Battle
            , learnCost = 0
            , actionPointCost = 1
            , magicPointCost = 0
            , subs =
                [ { target = Target.Enemy
                  , effects =
                    [ Effect.BattleFormula Formula.Attack
                    ]
                  }
                ]
            }
        
        "defend" ->
            { id = "defend"
            , name = "Defend"
            , context = Context.Battle
            , learnCost = 0
            , actionPointCost = 1
            , magicPointCost = 0
            , subs =
                [ { target = Target.Self
                  , effects =
                    [ Effect.BattleFormula Formula.Block
                    ]
                  }
                ]
            }
        
        "fireball" ->
            { id = "fireball"
            , name = "Fireball"
            , context = Context.Battle
            , learnCost = 1
            , actionPointCost = 1
            , magicPointCost = 1
            , subs =
                [ { target = Target.Enemy
                  , effects =
                    [ Effect.BattleFormula Formula.Fireball
                    ]
                  }
                ]
            }
        
        "heal" ->
            { id = "heal"
            , name = "Heal"
            , context = Context.Any
            , learnCost = 1
            , actionPointCost = 1
            , magicPointCost = 2
            , subs =
                [ { target = Target.Self
                  , effects =
                    [ Effect.BattleFormula Formula.Heal
                    ]
                  }
                ]
            }
        
        "chargeup" ->
            { id = "chargeup"
            , name = "Charge Up"
            , context = Context.Battle
            , learnCost = 20
            , actionPointCost = 1
            , magicPointCost = 0
            , subs =
                [ { target = Target.Self
                  , effects =
                    [ Effect.BattleFormula <| Formula.ChargeUp 1
                    ]
                  }
                ]
            }
        
        "chargeup4" ->
            { id = "chargeup4"
            , name = "Charge Up 4"
            , context = Context.Battle
            , learnCost = 20
            , actionPointCost = 1
            , magicPointCost = 0
            , subs =
                [ { target = Target.Self
                  , effects =
                    [ Effect.BattleFormula <| Formula.ChargeUp 4
                    ]
                  }
                ]
            }
        
        _ ->
            { id = "null"
            , name = "Null Action"
            , context = Context.None
            , actionPointCost = 0
            , learnCost = 0
            , magicPointCost = 0
            , subs = []
            }

learnable : List Action
learnable =
    [ byId "fireball"
    , byId "heal"
    , byId "chargeup"
    ]