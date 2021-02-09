module Terrain exposing
  ( Terrain(..)
  , all
  , generator
  )

import Random

import Region exposing (Region)

type Terrain
  = Plain
  | Swamp
  | Mountain
  | Lake
  | Forest

all : List Terrain
all =
  [ Plain
  , Swamp
  , Mountain
  , Lake
  , Forest
  ]