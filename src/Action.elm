module Action exposing
    ( Action
    , byId
    )

import ActionTag exposing (ActionTag)
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
    , tags : List ActionTag
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
    , tags = []
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
            , tags =
                [ ActionTag.Attack
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
            , tags = []
            }
        
        "attack" ->
            attackSkill "Attack" 1 1 Formula.Attack
        
        "tackle" ->
            { id = "tackle"
            , name = "Tackle"
            , context = Context.Battle
            , learnCost = 10
            , actionPointCost = 2
            , magicPointCost = 0
            , cooldown = 2
            , formulas =
                [ Formula.Attack
                ]
            , tags =
                [ ActionTag.Attack
                ]
            }
        
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
            , learnCost = 10
            , actionPointCost = 2
            , magicPointCost = 0
            , cooldown = 2
            , formulas =
                [ Formula.Block
                ]
            , tags = []
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
            , tags =
                [ ActionTag.Magic
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
            , tags =
                [ ActionTag.Magic
                ]
            }
        
        "focus-attack" ->
            { id = "focus-attack"
            , name = "Focus Attack"
            , context = Context.Battle
            , learnCost = 30
            , actionPointCost = 1
            , magicPointCost = 0
            , cooldown = 1
            , formulas =
                [ Formula.ChargeUp 1
                ]
            , tags = []
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
            , tags = []
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
            , tags = []
            }
        
        "half-fire" ->
            { id = "half-fire"
            , name = "Half Fire"
            , context = Context.Battle
            , learnCost = 5
            , actionPointCost = 2
            , magicPointCost = 1
            , cooldown = 2
            , formulas =
                [ Formula.HalfFire
                ]
            , tags =
                [ ActionTag.Magic
                ]
            }
        
        "flee" ->
            { id = "flee"
            , name = "Flee"
            , context = Context.Battle
            , learnCost = 1
            , actionPointCost = 3
            , magicPointCost = 0
            , cooldown = 1
            , formulas =
                [ Formula.Flee
                ]
            , tags = []
            }
        
        "magic-eating-bite" ->
            { id = "magic-eating-bite"
            , name = "Magic-Eating Bite"
            , context = Context.Battle
            , learnCost = 1
            , actionPointCost = 3
            , magicPointCost = 0
            , cooldown = 1
            , formulas =
                [ Formula.MagicEatingBite
                ]
            , tags =
                [ ActionTag.Attack
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
            , tags = []
            }