module Node exposing
  ( Node
  , generator
  , fake
  , toString
  )

import Random
import Array exposing (Array)

import MonsterTemplate exposing (MonsterTemplate)
import Shop exposing (Shop)
import Event exposing (Event)
import Terrain exposing (Terrain)
import SubRegion exposing (SubRegion)

import Map exposing (Map)
import NodeType exposing (NodeType)

type alias Node =
  { terrain : Terrain
  , type_ : NodeType
  , contents : Array NodeContent
  }

type alias NodeContent =
  { usage : NodeContentUsage
  , mobility : NodeContentMobility
  }

type NodeContentUsage
  = Limited Int
  | Unlimited

type NodeContentMobility
  = Mobile
  | Immobile

generator : Map -> Random.Generator Node
generator m =
  Terrain.generator m.subRegion
    |> Random.andThen (\terrain ->
      NodeType.generator m
        |> Random.andThen (\nodeType ->
          Random.constant <|
            { terrain = terrain
            , type_ = nodeType
            , contents = Array.empty
            }
        )
    )

fake : Node
fake =
  { terrain = Terrain.Plain
  , type_ = NodeType.Land
  , contents = Array.empty
  }

toString : Node -> String
toString n =
  "Terrain: " ++ Terrain.toString n.terrain
  ++ " | NodeType: " ++ NodeType.toString n.type_