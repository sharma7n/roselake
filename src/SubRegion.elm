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
  | LunarPalace

  -- Mintera (Dark/Poison/Luck/Psychic)
  | PyschoBayou
  | NoxiousAlley
  | RoyalePrison

  -- Suryasen (Faerie/Light/Steel/Eletric/Magic/Tech)
  | FaerieWyld
  | CloudGarden
  | MagitekAtelier

  -- Dragoceia (Water/Dragon/Prism)
  | ArcwyrmArchipelago
  | PrismaticDepths
  | DragonsCove

  -- Verterra (Grass/Ground/Bug???)
  | GiantJungle