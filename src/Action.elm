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
    let
        fireSkill level =
            { id = "fire" ++ String.fromInt level
            , name = "Fire " ++ String.fromInt level
            , context = Context.Battle
            , learnCost = 5 * (level + 1) * (level + 1)
            , actionPointCost = level
            , magicPointCost = (level + 1) * (level + 2)
            , subs =
                [ { target = Target.Enemy
                  , effects =
                    [ Effect.BattleFormula <| Formula.Fire level
                    ]
                  }
                ]
            }
        
        healSkill level =
            { id = "heal" ++ String.fromInt level
            , name = "Heal " ++ String.fromInt level
            , context = Context.Any
            , learnCost = 5 * (level + 1) * (level + 1)
            , actionPointCost = level
            , magicPointCost = (level + 1) * (level + 2)
            , subs =
                [ { target = Target.Self
                  , effects =
                    [ Effect.BattleFormula <| Formula.Heal level
                    ]
                  }
                ]
            }
    in
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
        
        "firebreath" ->
            { id = "firebreath"
            , name = "Fire Breath"
            , context = Context.Battle
            , learnCost = 1
            , actionPointCost = 1
            , magicPointCost = 1
            , subs =
                [ { target = Target.Enemy
                  , effects =
                    [ Effect.BattleFormula Formula.FireBreath
                    ]
                  }
                ]
            }
        
        "explode" ->
            { id = "explode"
            , name = "Explode"
            , context = Context.Battle
            , learnCost = 1
            , actionPointCost = 1
            , magicPointCost = 1
            , subs =
                [ { target = Target.Enemy
                  , effects =
                    [ Effect.BattleFormula Formula.Explode
                    ]
                  }
                ]
            }
        
        "fire0" ->
            fireSkill 0
        
        "fire1" ->
            fireSkill 1
        
        "fire2" ->
            fireSkill 2
        
        "fire3" ->
            fireSkill 3
        
        "fire4" ->
            fireSkill 4
        
        "heal0" ->
            healSkill 0
        
        "heal1"->
            healSkill 1
        
        "heal2"->
            healSkill 2
        
        "heal3"->
            healSkill 3
        
        "heal4" ->
            healSkill 4
        
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
        
        "chargeup2" ->
            { id = "chargeup2"
            , name = "Charge Up 2"
            , context = Context.Battle
            , learnCost = 10
            , actionPointCost = 1
            , magicPointCost = 0
            , subs =
                [ { target = Target.Self
                  , effects =
                    [ Effect.BattleFormula <| Formula.ChargeUp 2
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
        
        "curse" ->
            { id = "curse"
            , name = "Curse"
            , context = Context.Battle
            , learnCost = 0
            , actionPointCost = 1
            , magicPointCost = 1
            , subs =
                [ { target = Target.Enemy
                  , effects =
                    [ Effect.BattleFormula <| Formula.Curse
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
    [ byId "heal0"
    , byId "heal1"
    , byId "heal2"
    , byId "heal3"
    , byId "heal4"
    , byId "fire0"
    , byId "fire1"
    , byId "fire2"
    , byId "fire3"
    , byId "fire4"
    ]