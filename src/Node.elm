module Node exposing
  ( Node
  )

import MonsterTemplate exposing (MonsterTemplate)
import Shop exposing (Shop)
import Event exposing (Event)

type alias Node =
  { x : Int
  , y : Int
  , terrain : Terrain
  , content : NodeContent
  , terrainObjects : List TerrainObject
  , staticObjects : List StaticObject
  , dynamicObjects : List DynamicObject
  }

type Terrain
  = Plain
  | Swamp
  | Mountain
  | Hill
  | Cliff
  | Volcano
  | Island
  | Stream
  | River
  | Pond
  | Lake
  | Sea
  | Ocean
  | Underwater
  | Forest

type NodeContent
  = Land
  | Event Event
  | Shop Shop
  | Inn Int
  | SavePoint
  | MonsterNest MonsterTemplate
  | Goal