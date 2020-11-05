module CharacterCreationSettingSelection exposing
    ( CharacterCreationSettingSelection(..)
    )

import Build exposing (Build)
import Complexion exposing (Complexion)
import EyeColor exposing (EyeColor)
import HairColor exposing (HairColor)
import HairStyle exposing (HairStyle)
import Height exposing (Height)
import Weapon exposing (Weapon)
import Essentia exposing (Essentia)

type CharacterCreationSettingSelection
    = NameSelection String
    | HairStyleSelection HairStyle
    | HairColorSelection HairColor
    | EyeColorSelection EyeColor
    | ComplexionSelection Complexion
    | HeightSelection Height
    | BuildSelection Build
    | StartingWeaponSelection Weapon
    | StartingEssentiaSelection Essentia