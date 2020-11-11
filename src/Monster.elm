module Monster exposing
    ( Monster
    , new
    , generateReward
    )

import Set exposing (Set)
import Random

import Armor exposing (Armor)
import Behavior exposing (Behavior)
import MonsterTemplate exposing (MonsterTemplate)
import Reward exposing (Reward)
import Weapon exposing (Weapon)
import StatusSet exposing (StatusSet)
import Passive exposing (Passive)

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
    , vitality : Int
    , strength : Int
    , intellect : Int
    , charisma : Int
    , defense : Int
    , magicDefense : Int
    , agility : Int
    , behavior : Behavior
    , equippedWeapon : Maybe Weapon
    , equippedArmor : Maybe Armor
    , statusSet : StatusSet
    , block : Int
    , passives : List Passive
    , learned : Set String
    }

new : MonsterTemplate -> Monster
new t =
    { id = t.id
    , name = t.name
    , experience = t.experience
    , gold = t.gold
    , abilityPoints = t.abilityPoints
    , hitPoints = t.hitPoints
    , maxHitPoints = t.hitPoints
    , magicPoints = t.magicPoints
    , maxMagicPoints = t.magicPoints
    , actionPoints = 0
    , maxActionPoints = 0
    , vitality = t.vitality
    , strength = t.strength
    , defense = t.defense
    , magicDefense = t.magicDefense
    , intellect = t.intellect
    , charisma = t.charisma
    , agility = t.agility
    , behavior = t.behavior
    , equippedWeapon = t.equippedWeapon
    , equippedArmor = t.equippedArmor
    , statusSet = StatusSet.empty
    , block = 0
    , passives = t.passives
    , learned = Set.empty
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