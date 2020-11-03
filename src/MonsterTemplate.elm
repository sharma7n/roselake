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

base : MonsterTemplate
base =
    { id = "null"
    , name = "Null MonsterTemplate"
    , experience = 0
    , gold = 0
    , abilityPoints = 0
    , hitPoints = 1
    , magicPoints = 0
    , attack = 0
    , magic = 0
    , defense = 0
    , agility = 0
    , behavior = Behavior.None
    , equippedWeapon = Nothing
    , equippedArmor = Nothing
    }

byId : String -> MonsterTemplate
byId =
    Util.getById all base

all : List MonsterTemplate
all =
    let
        gremlin =
            { base
                | id = "gremlin"
                , experience = 1
                , gold = 1
                , abilityPoints = 1
                , hitPoints = 15
                , attack = 4
                , behavior = Behavior.Gremlin
            }

        divineDragon =
            { base
                | id = "divine-dragon"
                , experience = 2
                , gold = 2
                , abilityPoints = 2
                , hitPoints = 30
                , attack = 8
                , magic = 8
                , behavior = Behavior.DivineDragon
            }   
    in
    [ gremlin
    , divineDragon
    ]