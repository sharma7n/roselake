module Avatar exposing
    ( Avatar
    )

import HairStyle exposing (HairStyle)
import HairColor exposing (HairColor)
import EyeColor exposing (EyeColor)
import Complexion exposing (Complexion)
import Height exposing (Height)
import Build exposing (Build)

type alias Avatar =
    { hairStyle : HairStyle
    , hairColor : HairColor
    , eyeColor : EyeColor
    , complexion : Complexion
    , height : Height
    , build : Build
    }