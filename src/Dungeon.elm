module Dungeon exposing
    ( Dungeon
    , fromMap
    , generateMonsterTemplate
    , generateReward
    , describeCurrentPosition
    )

import Random

import Coordinates exposing (Coordinates)
import Grid exposing (Grid)

import Item exposing (Item)
import MonsterTemplate exposing (MonsterTemplate)
import Reward exposing (Reward)
import Node exposing (Node)
import Edge exposing (Edge)
import Map exposing (Map)

type alias Dungeon =
    { name : String
    , depth : Int
    , grid : Grid Node Edge
    , currentPosition : Coordinates
    }

fromMap : Map -> Dungeon
fromMap map =
    let
        node1 =
            Node.fake
        
        node2 =
            Node.fake
        
        edge =
            Edge.fake
    in
    { name = Map.name map
    , depth = 10
    , grid = Grid.fake node1 node2 edge
    , currentPosition = Coordinates.fake
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

describeCurrentPosition : Dungeon -> List String
describeCurrentPosition d =
    let
        fromNode n =
            "NODE OPTION"
        
        fromEdge e =
            "EDGE OPTION"
    in
    d.grid
        |> Grid.describeAt fromNode fromEdge d.currentPosition