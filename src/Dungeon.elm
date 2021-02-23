module Dungeon exposing
    ( Dungeon
    , generator
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

import SubRegion exposing (SubRegion)
import Node

type alias Dungeon =
    { map : Map
    , depth : Int
    , grid : Grid Node Edge
    , currentPosition : Coordinates
    }

generator : Map -> Random.Generator Dungeon
generator m =
    Node.generator m
        |> Random.andThen (\node1 ->
            Node.generator m
                |> Random.andThen (\node2 ->
                    Random.constant <|
                        { map = m
                        , depth = 10
                        , grid = Grid.fake node1 node2 Edge.fake
                        , currentPosition = Coordinates.fake
                        }
                )
        )

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
    { map = map
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
        fromEdge e =
            "EDGE OPTION"
    in
    d.grid
        |> Grid.describeAt Node.toString fromEdge d.currentPosition