module Action exposing
    ( Action
    , byId
    , learnable
    )

import Context exposing (Context)
import Formula exposing (Formula)

import Util

type alias Action =
    { id : String
    , name : String
    , context : Context
    , learnCost : Int
    , actionPointCost : Int
    , magicPointCost : Int
    , cooldown : Int
    , formulas : List Formula
    }

base : Action
base =
    { id = "null"
    , name = "Null Action"
    , context = Context.None
    , learnCost = 0
    , actionPointCost = 0
    , magicPointCost = 0
    , cooldown = 0
    , formulas = []
    }

byId : String -> Action
byId id =
    let
        attackSkill name actionPointCost cooldown formula =
            { id = Util.kebabify name
            , name = name
            , context = Context.Battle
            , learnCost = 0
            , actionPointCost = actionPointCost
            , magicPointCost = 0
            , cooldown = cooldown
            , formulas =
                [ formula
                ]
            }
        
        fireSkill level =
            { id = "fire" ++ String.fromInt level
            , name = "Fire " ++ String.fromInt level
            , context = Context.Battle
            , learnCost = 5 * (level + 1) * (level + 1)
            , actionPointCost = level
            , magicPointCost = (level + 1) * (level + 2)
            , cooldown = 1
            , formulas =
                [ Formula.Fire level
                ]
            }
        
        healSkill level =
            { id = "heal" ++ String.fromInt level
            , name = "Heal " ++ String.fromInt level
            , context = Context.Any
            , learnCost = 5 * (level + 1) * (level + 1)
            , actionPointCost = level
            , magicPointCost = (level + 1) * (level + 2)
            , cooldown = 1
            , formulas =
                [ Formula.Heal level
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
            , cooldown = 0
            , formulas = []
            }
        
        "attack" ->
            attackSkill "Attack" 1 1 Formula.Attack
        
        "tackle" ->
            attackSkill "Tackle" 2 2 Formula.Attack
        
        "axe-attack" ->
            attackSkill "Axe Attack" 3 3 Formula.AxeAttack
        
        "bow-attack" ->
            attackSkill "Bow Attack" 2 2 Formula.BowAttack
        
        "claw-attack" ->
            attackSkill "Claw Attack" 1 1 Formula.ClawAttack
        
        "staff-attack" ->
            attackSkill "Staff Attack" 2 2 Formula.StaffAttack
        
        "defend" ->
            { id = "defend"
            , name = "Defend"
            , context = Context.Battle
            , learnCost = 0
            , actionPointCost = 1
            , magicPointCost = 0
            , cooldown = 1
            , formulas =
                [ Formula.Block
                ]
            }
        
        "mega-flare" ->
            { id = "mega-flare"
            , name = "Mega Flare"
            , context = Context.Battle
            , learnCost = 1
            , actionPointCost = 1
            , magicPointCost = 1
            , cooldown = 1
            , formulas =
                [ Formula.MegaFlare
                ]
            }
        
        "explode" ->
            { id = "explode"
            , name = "Explode"
            , context = Context.Battle
            , learnCost = 1
            , actionPointCost = 1
            , magicPointCost = 1
            , cooldown = 1
            , formulas =
                [ Formula.Explode
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
        
        "buff-attack" ->
            { id = "buff-attack"
            , name = "Buff Attack"
            , context = Context.Battle
            , learnCost = 20
            , actionPointCost = 1
            , magicPointCost = 0
            , cooldown = 1
            , formulas =
                [ Formula.ChargeUp 1
                ]
            }
        
        "curse" ->
            { id = "curse"
            , name = "Curse"
            , context = Context.Battle
            , learnCost = 0
            , actionPointCost = 1
            , magicPointCost = 1
            , cooldown = 1
            , formulas =
                [ Formula.Curse
                ]
            }
        
        "poison" ->
            { id = "poison"
            , name = "Poison"
            , context = Context.Battle
            , learnCost = 1
            , actionPointCost = 1
            , magicPointCost = 1
            , cooldown = 1
            , formulas =
                [ Formula.Poison
                ]
            }
        
        _ ->
            { id = "null"
            , name = "Null Action"
            , context = Context.None
            , actionPointCost = 0
            , learnCost = 0
            , magicPointCost = 0
            , cooldown = 0
            , formulas = []
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