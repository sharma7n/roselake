module SubRegion exposing
  ( SubRegion(..)
  , all
  , toString
  , generator 
  )

import Random

import CustomDict exposing (CustomDict)

import Region exposing (Region)

type SubRegion =
  | NullSubRegion

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
  [ NullSubRegion
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
  , ProtegeaRuins
  , EschatonsZiggurat
  , StarRoad
  , RainbowRoad
  , SpecialZone
  ]

toString : SubRegion -> String
toString s =
  case s of
    NullSubRegion ->
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
    
    ProtegeaRuins ->
      "Protogea Ruins"
    
    EschatonsZiggurat ->
      "Eschaton's Ziggurat"
    
    StarRoad ->
      "Star Road"
    
    RainbowRoad ->
      "Rainbow Road"
    
    SpecialZone ->
      "Special Zone"

newCustomDict : CustomDict SubRegion a
newCustomDict =
  CustomDict.new toString

generator : Region -> CustomDict SubRegion () -> Random.Generator SubRegion
generator region unlocked =
  let
    generateAvailable rps =
      let
        available =
          rps
            |> List.filter (\(r, p) -> Dict.member r unlocked)
      in
      Random.weighted ( 0, NullSubRegion ) rps
  in
  case region of
    Estrel ->
      generateAvailable
        [ ( MeteoriteHill, 70 )
        , ( RimefireCave, 20 )
        , ( SiderealPeak, 10 )
        ]

    Himin ->
      generateAvailable
        [ ( BurningPlains, 70 )
        , ( AncientCemetary, 20 )
        , ( BloodMoonPalace, 10 )
        ]

    Mintera ->
      generateAvailable
        [ ( PandemonicBayou, 70 )
        , ( OathbreakersAlley, 20 )
        , ( MisfortunesStockade, 10 )
        ]

    Suryasen ->
      generateAvailable
        [ ( FaerieWyld, 70 )
        , ( CloudGarden, 20 )
        , ( MagitekAtelier, 10 )
        ]

    Dragoceia ->
      generateAvailable
        [ ( ArchwyrmArchipelago, 70 )
        , ( PrismaticDepths, 20 )
        , ( DragonGodsCove, 10 )
        ]

    Sanarbol ->
      generateAvailable
        [ ( GigantJungle, 70 )
        , ( ProtegeaRuins, 20 )
        , ( EschatonsZiggurat, 10 )
        ]