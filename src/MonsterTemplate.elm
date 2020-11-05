module MonsterTemplate exposing
    ( MonsterTemplate
    , byId
    )

import Util

import Armor exposing (Armor)
import Behavior exposing (Behavior)
import Weapon exposing (Weapon)

type alias MonsterTemplate =
    { id : String
    , name : String
    , experience : Int
    , gold : Int
    , abilityPoints : Int
    , hitPoints : Int
    , magicPoints : Int
    , attack : Int
    , magic : Int
    , defense : Int
    , agility : Int
    , behavior : Behavior
    , equippedWeapon : Maybe Weapon
    , equippedArmor : Maybe Armor
    }

type alias Extra =
    { experience : Int
    , gold : Int
    , abilityPoints : Int
    , hitPoints : Int
    , magicPoints : Int
    , attack : Int
    , magic : Int
    , defense : Int
    , agility : Int
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
    , attack = 0
    , magic = 0
    , defense = 0
    , agility = 0
    , behavior = behavior
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
                , attack = 4
                , magic = 4
            })
    , new "Bun Bun" Behavior.BunBun
        (\m ->
            { m
                | experience = 2
                , gold = 2
                , abilityPoints = 1
                , hitPoints = 20
                , attack = 6
                , magic = 0
            })
    , new "Wyvern" Behavior.Wyvern
        (\m ->
            { m
                | experience = 3
                , gold = 3
                , abilityPoints = 3
                , hitPoints = 30
                , attack = 8
                , magic = 8
            })
    ]