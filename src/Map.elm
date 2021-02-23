module Map exposing
  ( Map
  , name
  )

import Random

import SubRegion exposing (SubRegion)

type alias Map =
  { seed : Random.Seed
  , subRegion : SubRegion
  , level : Int
  }

name : Map -> String
name m =
    SubRegion.toString m.subRegion ++ " Lv. " ++ String.fromInt m.level