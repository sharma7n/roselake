module Dungeon exposing
    ( Dungeon
    , byId
    , generateReward
    )

import Random

import Item exposing (Item)
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