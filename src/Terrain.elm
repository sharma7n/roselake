module Terrain exposing
  ( Terrain(..)
  , all
  , toString
  , generator
  )

import Random

import SubRegion exposing (SubRegion)

type Terrain
  = Null
  | Plain
  | Swamp
  | Mountain
  | Lake
  | Forest

all : List Terrain
all =
  [ Null
  , Plain
  , Swamp
  , Mountain
  , Lake
  , Forest
  ]

toString : Terrain -> String
toString t =
  case t of
    Null ->
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
    SubRegion.Null ->
      Random.constant Null

    SubRegion.MeteoriteHill ->
      Random.constant Null
    
    SubRegion.RimefireCave ->
      Random.constant Null
    
    SubRegion.SiderealPeak ->
      Random.constant Null
    
    SubRegion.BurningPlains ->
      Random.constant Null
    
    SubRegion.AncientCemetary ->
      Random.constant Null
    
    SubRegion.BloodMoonPalace ->
      Random.constant Null
    
    SubRegion.PandemonicBayou ->
      Random.constant Null
    
    SubRegion.OathbreakersAlley ->
      Random.constant Null
    
    SubRegion.MisfortunesStockade ->
      Random.constant Null
    
    SubRegion.FaerieWyld ->
      Random.constant Null
    
    SubRegion.CloudGarden ->
      Random.constant Null
    
    SubRegion.MagitekAtelier ->
      Random.constant Null
    
    SubRegion.ArchwyrmArchipelago ->
      Random.constant Null
    
    SubRegion.PrismaticDepths ->
      Random.constant Null
    
    SubRegion.DragonGodsCove ->
      Random.constant Null
    
    SubRegion.GigantJungle ->
      Random.constant Null
    
    SubRegion.ProtogeaRuins ->
      Random.constant Null
    
    SubRegion.EschatonsZiggurat ->
      Random.constant Null
    
    SubRegion.StarRoad ->
      Random.constant Null
    
    SubRegion.RainbowRoad ->
      Random.constant Null
    
    SubRegion.SpecialZone ->
      Random.constant Null