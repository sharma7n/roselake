module Dungeon exposing
    ( Dungeon
    , byId
    , generateMonsterTemplate
    , generateReward
    )

import Random

import Item exposing (Item)
import MonsterTemplate exposing (MonsterTemplate)
import Reward exposing (Reward)

type alias Dungeon =
    { name : String
    , depth : Int
    }

byId : String -> Dungeon
byId id =
    case id of
        "beginnerscave" ->
            { name = "Beginner's Cave"
            , depth = 10
            }
        
        _ ->
            { name = "Null Dungeon"
            , depth = 0
            }

generateMonsterTemplate : Dungeon -> Random.Generator MonsterTemplate
generateMonsterTemplate _ =
    Random.weighted
        ( 0, MonsterTemplate.byId "" )
        [ ( 100, MonsterTemplate.byId "gremlin" )
        ]

generateReward : Dungeon -> Random.Generator Reward
generateReward _ =
    Random.constant <|
        { experience = 5
        , gold = 5
        , abilityPoints = 5
        , items =
            [ ( Item.byId "potion", 3 )
            ]
        , weapons =
            []
        , armors =
            []
        }