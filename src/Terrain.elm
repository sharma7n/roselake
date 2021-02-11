module Terrain exposing
  ( Terrain(..)
  , all
  , toString
  , generator
  )

import Random

import SubRegion exposing (SubRegion)

type Terrain
  = NullTerrain
  | Plain
  | Swamp
  | Mountain
  | Lake
  | Forest

all : List Terrain
all =
  [ NullTerrain
  , Plain
  , Swamp
  , Mountain
  , Lake
  , Forest
  ]

toString : Terrain -> String
toString t =
  case t of
    NullTerrain ->
      "Null Terrain"
    
    Plain ->
      "Plain"
    
    Swamp ->
      "Swamp"
    
    Mountain ->
      "Mountain"
    
    Lake ->
      "Lake"
    
    Forest ->
      "Forest"

generator : SubRegion -> Random.Generator Terrain
generator s =
  case s of
    NullSubRegion ->
      Random.constant NullTerrain

    MeteoriteHill ->
      Random.constant NullTerrain
    
    RimefireCave ->
      Random.constant NullTerrain
    
    SiderealPeak ->
      Random.constant NullTerrain
    
    BurningPlains ->
      Random.constant NullTerrain
    
    AncientCemetary ->
      Random.constant NullTerrain
    
    BloodMoonPalace ->
      Random.constant NullTerrain
    
    PandemonicBayou ->
      Random.constant NullTerrain
    
    OathbreakersAlley ->
      Random.constant NullTerrain
    
    MisfortunesStockade ->
      Random.constant NullTerrain
    
    FaerieWyld ->
      Random.constant NullTerrain
    
    CloudGarden ->
      Random.constant NullTerrain
    
    MagitekAtelier ->
      Random.constant NullTerrain
    
    ArchwyrmArchipelago ->
      Random.constant NullTerrain
    
    PrismaticDepths ->
      Random.constant NullTerrain
    
    DragonGodsCove ->
      Random.constant NullTerrain
    
    GigantJungle ->
      Random.constant NullTerrain
    
    ProtegeaRuins ->
      Random.constant NullTerrain
    
    EschatonsZiggurat ->
      Random.constant NullTerrain
    
    StarRoad ->
      Random.constant NullTerrain
    
    RainbowRoad ->
      Random.constant NullTerrain
    
    SpecialZone ->
      Random.constant NullTerrain