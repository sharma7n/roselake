module Monster exposing
    ( Monster
    , byId
    , generateReward
    )

import Random

import Util

import Distribution exposing (Distribution)

import Action exposing (Action)
import Behavior exposing (Behavior)
import Armor exposing (Armor)
import Reward exposing (Reward)
import Status exposing (Status)
import Weapon exposing (Weapon)

import StatusSet exposing (StatusSet)

type alias Monster =
    { id : String
    , name : String
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
    , behavior : Behavior
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
    { id = "null"
    , name = "Null Monster"
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
    , behavior = Behavior.None
    , equippedWeapon = Nothing
    , equippedArmor = Nothing
    , statusSet = StatusSet.empty
    , block = 0
    }

byId : String -> Monster
byId =
    Util.getById all base

all : List Monster
all =
    let
        slime =
            { base
                | id = "slime"
                , experience = 1
                , gold = 1
                , abilityPoints = 1
                , hitPoints = 15
                , maxHitPoints = 15
                , attack = 4
                , behavior = Behavior.Slime
            }
    in
    [ slime
    ]