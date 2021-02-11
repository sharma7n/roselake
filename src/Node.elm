module Node exposing
  ( Node
  )

import Array exposing (Array)

import MonsterTemplate exposing (MonsterTemplate)
import Shop exposing (Shop)
import Event exposing (Event)
import Terrain exposing (Terrain)

type alias Node =
  { x : Int
  , y : Int
  , terrain : Terrain
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