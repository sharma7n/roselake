module MonsterTemplate exposing
    ( MonsterTemplate
    , byId
    )

import Util

import Armor exposing (Armor)
import Behavior exposing (Behavior)
import Weapon exposing (Weapon)
import Passive exposing (Passive)

type alias MonsterTemplate =
    { id : String
    , name : String
    , experience : Int
    , gold : Int
    , abilityPoints : Int
    , hitPoints : Int
    , magicPoints : Int
    , vitality : Int
    , attack : Int
    , magic : Int
    , defense : Int
    , magicDefense : Int
    , agility : Int
    , behavior : Behavior
    , passives : List Passive
    , equippedWeapon : Maybe Weapon
    , equippedArmor : Maybe Armor
    }

new : String -> Behavior -> (MonsterTemplate -> MonsterTemplate) -> MonsterTemplate
new name behavior f =
    { id = Util.kebabify name
    , name = name
    , experience = 0
    , gold = 0
    , abilityPoints = 0
    , hitPoints = 1
    , magicPoints = 0
    , vitality = 0
    , attack = 0
    , magic = 0
    , defense = 0
    , magicDefense = 0
    , agility = 0
    , behavior = behavior
    , passives = []
    , equippedWeapon = Nothing
    , equippedArmor = Nothing
    }
        |> f

byId : String -> MonsterTemplate
byId =
    let
        default =
            new "Null" Behavior.None (\x -> x)
    in
    Util.getById all  default

all : List MonsterTemplate
all =
    [ new "Dummy" Behavior.Dummy 
        (\m ->
            { m
                | hitPoints = 100
            })
    , new "Gremlin" Behavior.Gremlin
        (\m ->
            { m
                | experience = 1
                , gold = 1
                , abilityPoints = 1
                , hitPoints = 15
                , vitality = 1
                , attack = 4
                , magic = 4
            })
    , new "Magic-Eating Tortoise" Behavior.MagicEatingTortoise
        (\m ->
            { m
                | hitPoints = 30
                , vitality = 3
                , attack = 2
                , magic = 8
                , passives =
                    [ Passive.byId "p-counter:-focus-defense"
                    , Passive.byId "m-counter:-learn-spell"
                    ]
            }
        )
    , new "Wyvern" Behavior.Wyvern
        (\m ->
            { m
                | experience = 1
                , gold = 1
                , abilityPoints = 1
                , hitPoints = 30
                , vitality = 2
                , attack = 8
                , magic = 8
            })
    ]