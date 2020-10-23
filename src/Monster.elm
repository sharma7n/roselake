module Monster exposing
    ( Monster
    , byId
    , generateReward
    )

import Random

import Distribution exposing (Distribution)

import Action exposing (Action)
import Behavior exposing (Behavior)
import Armor exposing (Armor)
import Reward exposing (Reward)
import Status exposing (Status)
import Weapon exposing (Weapon)

import StatusSet exposing (StatusSet)

type alias Monster =
    { name : String
    , experience : Int
    , gold : Int
    , abilityPoints : Int
    , hitPoints : Int
    , maxHitPoints : Int
    , magicPoints : Int
    , maxMagicPoints : Int
    , actionPoints : Int
    , maxActionPoints : Int
    , attack : Int
    , magic : Int
    , defense : Int
    , agility : Int
    , behaviors : List Behavior
    , equippedWeapon : Maybe Weapon
    , equippedArmor : Maybe Armor
    , statusSet : StatusSet
    , block : Int
    }

generateReward : Monster -> Random.Generator Reward
generateReward monster =
    Random.constant <|
        { experience = monster.experience
        , gold = monster.gold
        , abilityPoints = monster.abilityPoints
        , items = []
        , weapons = []
        , armors = []
        }

base : Monster
base =
    { name = "Null Monster"
    , experience = 0
    , gold = 0
    , abilityPoints = 0
    , hitPoints = 1
    , maxHitPoints = 1
    , magicPoints = 1
    , maxMagicPoints = 1
    , actionPoints = 0
    , maxActionPoints = 0
    , attack = 0
    , magic = 0
    , defense = 0
    , agility = 0
    , behaviors = []
    , equippedWeapon = Nothing
    , equippedArmor = Nothing
    , statusSet = StatusSet.empty
    , block = 0
    }

byId : String -> Monster
byId id =
    case id of
        "slime" ->
            { base
                | name = "Slime"
                , experience = 1
                , gold = 1
                , abilityPoints = 1
                , hitPoints = 15
                , maxHitPoints = 15
                , attack = 4
                , behaviors =
                    let
                        primary = 
                            Behavior.new 5 Behavior.Always <|
                                Distribution.new
                                    ( 50, Action.byId "nothing" )
                                    [ ( 50, Action.byId "attack" )
                                    ]
                    in
                    [ primary
                    ]
            }
        
        "ghost" ->
            { base
                | name = "Ghost"
                , behaviors =
                    let
                        curse =
                            Behavior.new 5 Behavior.Always <|
                                Distribution.new
                                    ( 50, Action.byId "curse" )
                                    []
                    in
                    [ curse
                    ]
            }
        
        "wolf" ->
            { base
                | name = "Wolf"
                , experience = 2
                , gold = 1
                , abilityPoints = 1
                , hitPoints = 20
                , maxHitPoints = 20
                , attack = 5
                , behaviors =
                    let
                        primary = 
                            Behavior.new 5 Behavior.Always <|
                                Distribution.new
                                    ( 50, Action.byId "attack" )
                                    [ ( 50, Action.byId "chargeup2" )
                                    ]
                    in
                    [ primary
                    ]
            }
        
        "bomb" ->
            { base
                | name = "Bomb"
                , experience = 3
                , gold = 2
                , abilityPoints = 2
                , hitPoints = 30
                , maxHitPoints = 30
                , magicPoints = 60
                , maxMagicPoints = 60
                , magic = 2
                , behaviors =
                    let
                        primary = 
                            Behavior.new 5 Behavior.Always <|
                                Distribution.new
                                    ( 50, Action.byId "nothing" )
                                    [ ( 50, Action.byId "fire0" )
                                    ]
                        
                        explode =
                            Behavior.new 10 (Behavior.BelowHitPointThreshold 0.5) <|
                                Distribution.new
                                    ( 100, Action.byId "explode" )
                                    []
                    in
                    [ primary
                    , explode
                    ]
            }
        
        "dummy" ->
            { base
                | name = "Dummy"
                , hitPoints = 10
                , maxHitPoints = 10
                , behaviors =
                    let
                        primary =
                            Behavior.new 5 Behavior.Always <|
                                Distribution.new
                                    ( 100, Action.byId "nothing" )
                                    []

                    in
                    [ primary
                    ]
            }
        
        "gremlin" ->
            { base
                | name = "Gremlin"
                , experience = 1
                , gold = 1
                , abilityPoints = 1
                , hitPoints = 24
                , maxHitPoints = 24
                , magicPoints = 8
                , maxMagicPoints = 8
                , attack = 4
                , magic = 2
                , defense = 4
                , behaviors =
                    let
                        primary =
                            Behavior.new 5 Behavior.Always <|
                                Distribution.new
                                    ( 50, Action.byId "attack" )
                                    [ ( 50, Action.byId "defend" )
                                    ]
                        
                        fireball =
                            Behavior.new 10 (Behavior.RoundSchedule 1 4) <|
                                Distribution.new
                                    ( 100, Action.byId "fireball" )
                                    []
                    in
                    [ primary
                    , fireball
                    ]
            }
        
        "dragon" ->
            { base
                | name = "Dragon"
                , experience = 10
                , gold = 10
                , abilityPoints = 10
                , hitPoints = 60
                , maxHitPoints = 60
                , magicPoints = 20
                , maxMagicPoints = 20
                , attack = 3
                , magic = 3
                , behaviors =
                    let
                        spike =
                            Behavior.new 10 (Behavior.RoundSchedule 1 6) <|
                                Distribution.new
                                    ( 100, Action.byId "firebreath" )
                                    []
                        
                        attrition =
                            Behavior.new 5 Behavior.Always <|
                                Distribution.new
                                    ( 100, Action.byId "attack" )
                                    []
                        
                    in
                    [ spike
                    , attrition
                    ]
            }
        
        _ ->
            base