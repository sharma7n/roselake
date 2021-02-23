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

type alias Node =
  { terrain : Terrain
  , type_ : NodeType
  , contents : Array NodeContent
  }

type NodeType
  = Land
  | Event Event
  | Shop Shop
  | Inn Int
  | SavePoint
  | MonsterNest MonsterTemplate
  | Goal

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

generator : SubRegion -> Random.Generator Node
generator s =
  Random.constant <|
    { terrain = Terrain.Null
    , type_ = Land
    , contents = Array.empty
    }

fake : Node
fake =
  { terrain = Terrain.Plain
  , type_ = Land
  , contents = Array.empty
  }

toString : Node -> String
toString n =
  "Terrain: " ++ Terrain.toString n.terrain