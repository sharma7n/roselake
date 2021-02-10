module SubRegion exposing
  ( SubRegion(..)
  , all
  , generator 
  )

import Random

import Region exposing (Region)

type SubRegion =
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
  | MisfortunesPrison

  -- Suryasen (Faerie/Light/Steel/Eletric/Magic/Tech)
  | FaerieWyld
  | CloudGarden
  | MagitekAtelier

  -- Dragoceia (Water/Dragon/Prism)
  | ArchwyrmArchipelago
  | PrismaticDepths
  | DragonGodIsland

  -- Verterra (Grass/Ground/Illusion/Time/Bug/Dinosaur/???)
  | GiantJungle
  | PrimevalRuins
  | EonsZiggurat

  -- Other
  | PhantomSkyship