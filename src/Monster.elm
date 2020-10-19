module Monster exposing
    ( Monster
    , byId
    , generator
    , chooseAction
    , generateReward
    )

import Random

import Distribution exposing (Distribution)

import Action exposing (Action)
import Armor exposing (Armor)
import Reward exposing (Reward)
import Status exposing (Status)
import Weapon exposing (Weapon)

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
    , actions : Distribution Action
    , equippedWeapon : Maybe Weapon
    , equippedArmor : Maybe Armor
    , statuses : List Status
    }

byId : String -> Monster
byId id =
    case id of
        "slime" ->
            { name = "Slime"
            , experience = 1
            , gold = 1
            , abilityPoints = 1
            , hitPoints = 3
            , maxHitPoints = 3
            , magicPoints = 0
            , maxMagicPoints = 0
            , actionPoints = 0
            , maxActionPoints = 0
            , attack = 1
            , magic = 1
            , defense = 0
            , agility = 1
            , actions =
                Distribution.new
                    ( 50, Action.byId "nothing" )
                    [ ( 50, Action.byId "attack" )
                    ]
            , equippedWeapon = Nothing
            , equippedArmor = Nothing
            , statuses = []
            }
        
        "dummy" ->
            { name = "Dummy"
            , experience = 1
            , gold = 1
            , abilityPoints = 1
            , hitPoints = 10
            , maxHitPoints = 10
            , magicPoints = 1
            , maxMagicPoints = 1
            , actionPoints = 0
            , maxActionPoints = 0
            , attack = 1
            , magic = 1
            , defense = 0
            , agility = 1
            , actions = Distribution.new
                ( 1, Action.byId "nothing" )
                []
            , equippedWeapon = Nothing
            , equippedArmor = Nothing
            , statuses = []
            }
        
        "gremlin" ->
            { name = "Gremlin"
            , experience = 1
            , gold = 1
            , abilityPoints = 1
            , hitPoints = 24
            , maxHitPoints = 24
            , magicPoints = 8
            , maxMagicPoints = 8
            , actionPoints = 0
            , maxActionPoints = 0
            , attack = 4
            , magic = 2
            , defense = 0
            , agility = 1
            , actions =
                Distribution.new
                    ( 25, Action.byId "attack" )
                    [ ( 25, Action.byId "fireball" )
                    , ( 25, Action.byId "heal" )
                    , ( 25, Action.byId "chargeup4" )
                    ]
            , equippedWeapon = Nothing
            , equippedArmor = Nothing
            , statuses = []
            }
        
        _ ->
            { name = "Null Monster"
            , experience = 0
            , gold = 0
            , abilityPoints = 0
            , hitPoints = 0
            , maxHitPoints = 0
            , magicPoints = 0
            , maxMagicPoints = 0
            , actionPoints = 0
            , maxActionPoints = 0
            , attack = 0
            , magic = 0
            , defense = 0
            , agility = 0
            , actions =
                Distribution.new
                    ( 0, Action.byId "null" )
                    []
            , equippedWeapon = Nothing
            , equippedArmor = Nothing
            , statuses = []
            }

generator : Random.Generator Monster
generator =
    Random.weighted
        ( 0, byId "" )
        [ ( 1, byId "slime" )
        ]

chooseAction : Monster -> Random.Generator Action
chooseAction monster =
    chooseActionInternal monster 0

chooseActionInternal : Monster -> Int -> Random.Generator Action
chooseActionInternal monster retries =
    if retries > 2 then
        Random.constant <| Action.byId "attack"
    else
        Distribution.random monster.actions
            |> Random.andThen (\action ->
                if action.magicPointCost <= monster.magicPoints then
                    Random.constant action
                else
                    chooseActionInternal monster (retries + 1)
            )

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