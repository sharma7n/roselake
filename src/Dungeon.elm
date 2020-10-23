module Dungeon exposing
    ( Dungeon
    , byId
    , generateMonster
    , generateReward
    )

import Random

import Item exposing (Item)
import Monster exposing (Monster)
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

generateMonster : Dungeon -> Random.Generator Monster
generateMonster _ =
    Random.weighted
        ( 0, Monster.byId "" )
        [ ( 20, Monster.byId "slime" )
        , ( 20, Monster.byId "wolf" )
        , ( 20, Monster.byId "bomb" )
        , ( 20, Monster.byId "ghost" )
        , ( 2000, Monster.byId "spider" )
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