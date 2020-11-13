module Boss exposing
    ( Boss
    , byId
    )

import Set exposing (Set)

import Util
import Weapon exposing (Weapon)
import Armor exposing (Armor)
import StatusSet exposing (StatusSet)
import Passive exposing (Passive)

type alias Boss =
    { hitPoints : Int
    , maxHitPoints : Int
    , magicPoints : Int
    , maxMagicPoints : Int
    , actionPoints : Int
    , maxActionPoints : Int
    , vitality : Int
    , strength : Int
    , intellect : Int
    , agility : Int
    , defense : Int
    , charisma : Int
    , magicDefense : Int
    , equippedWeapon : Maybe Weapon
    , equippedArmor : Maybe Armor
    , statusSet : StatusSet
    , block : Int
    , passives : List Passive
    , learned : Set String
    , id : String
    , name : String
    , depth : Int
    }

default : Boss
default =
    { hitPoints = 1
    , maxHitPoints = 1
    , magicPoints = 0
    , maxMagicPoints = 0
    , actionPoints = 0
    , maxActionPoints = 0
    , vitality = 0
    , strength = 0
    , intellect = 0
    , agility = 0
    , defense = 0
    , charisma = 0
    , magicDefense = 0
    , equippedWeapon = Nothing
    , equippedArmor = Nothing
    , statusSet = StatusSet.empty
    , block = 0
    , passives = []
    , learned = Set.empty
    , id = "null"
    , name = "Null Boss"
    , depth = 0
    }

byId : String -> Boss
byId =
    Util.getById all default

all : List Boss
all =
    let
        ogopogo =
            { default
                | id = "ogopogo"
                , name = "Ogopogo"
            }
    in
    [ ogopogo
    ]