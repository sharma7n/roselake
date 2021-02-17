module SubRegion exposing
  ( SubRegion(..)
  , all
  , toString
  , generator 
  )

import Random

import CustomDict exposing (CustomDict)

import Region exposing (Region)

type SubRegion
  = Null

  -- Estrel (Ice/Space)
  | MeteoriteHill
  | RimefireCave
  | SiderealPeak

  -- Himin (Fire/Ghost/Moon)
  | BurningPlains
  | AncientCemetary
  | BloodMoonPalace

  -- Mintera (Luck/Psychic/Chaos/Dark/Poison)
  | PandemonicBayou
  | OathbreakersAlley
  | MisfortunesStockade

  -- Suryasen (Faerie/Light/Steel/Eletric/Magic/Tech)
  | FaerieWyld
  | CloudGarden
  | MagitekAtelier

  -- Dragoceia (Water/Dragon/Prism)
  | ArchwyrmArchipelago
  | PrismaticDepths
  | DragonGodsCove


  -- Sanarbol (Grass/Ground/Illusion/Time/Bug/Dinosaur/???)
  | GigantJungle
  | ProtogeaRuins
  | EschatonsZiggurat

  -- Other
  | StarRoad
  | RainbowRoad
  | SpecialZone

all : List SubRegion
all =
  [ Null
  , MeteoriteHill
  , RimefireCave
  , SiderealPeak
  , BurningPlains
  , AncientCemetary
  , BloodMoonPalace
  , PandemonicBayou
  , OathbreakersAlley
  , MisfortunesStockade
  , FaerieWyld
  , CloudGarden
  , MagitekAtelier
  , ArchwyrmArchipelago
  , PrismaticDepths
  , DragonGodsCove
  , GigantJungle
  , ProtogeaRuins
  , EschatonsZiggurat
  , StarRoad
  , RainbowRoad
  , SpecialZone
  ]

toString : SubRegion -> String
toString s =
  case s of
    Null ->
      "Null Sub-Region"
    
    MeteoriteHill ->
      "Meteorite Hill"
    
    RimefireCave ->
      "Rimefire Cave"
    
    SiderealPeak ->
      "Sidereal Peak"
    
    BurningPlains ->
      "Burning Plains"
    
    AncientCemetary ->
      "Ancient Cemetary"
    
    BloodMoonPalace ->
      "Blood-Moon Palace"
    
    PandemonicBayou ->
      "Pandemonic Bayou"
    
    OathbreakersAlley ->
      "Oathbreaker's Alley"
    
    MisfortunesStockade ->
      "Misfortune's Stockade"
    
    FaerieWyld ->
      "Faerie Wyld"
    
    CloudGarden ->
      "Cloud Garden"
    
    MagitekAtelier ->
      "Magitek Atelier"
    
    ArchwyrmArchipelago ->
      "Archwyrm Archipelago"
    
    PrismaticDepths ->
      "Prismatic Depths"
    
    DragonGodsCove ->
      "Dragon God's Cove"
    
    GigantJungle ->
      "Gigant Jungle"
    
    ProtogeaRuins ->
      "Protogea Ruins"
    
    EschatonsZiggurat ->
      "Eschaton's Ziggurat"
    
    StarRoad ->
      "Star Road"
    
    RainbowRoad ->
      "Rainbow Road"
    
    SpecialZone ->
      "Special Zone"

generator : Region -> CustomDict SubRegion () -> Random.Generator SubRegion
generator region unlocked =
  let
    generateAvailable rps =
      let
        available =
          rps
            |> List.filter (\(p, r) -> CustomDict.member toString r unlocked)
      in
      Random.weighted ( 0, Null ) rps
  in
  case region of
    Region.Estrel ->
      generateAvailable
        [ ( 70, MeteoriteHill )
        , ( 20, RimefireCave )
        , ( 10, SiderealPeak )
        ]

    Region.Himin ->
      generateAvailable
        [ ( 70, BurningPlains )
        , ( 20, AncientCemetary )
        , ( 10, BloodMoonPalace )
        ]

    Region.Mintera ->
      generateAvailable
        [ ( 70, PandemonicBayou )
        , ( 20, OathbreakersAlley )
        , ( 10, MisfortunesStockade )
        ]

    Region.Suryasen ->
      generateAvailable
        [ ( 70, FaerieWyld )
        , ( 20, CloudGarden )
        , ( 10, MagitekAtelier )
        ]

    Region.Dragoceia ->
      generateAvailable
        [ ( 70, ArchwyrmArchipelago )
        , ( 20, PrismaticDepths )
        , ( 10, DragonGodsCove )
        ]

    Region.Sanarbol ->
      generateAvailable
        [ ( 70, GigantJungle )
        , ( 20, ProtogeaRuins )
        , ( 10, EschatonsZiggurat )
        ]