module Avatar exposing
    ( Avatar
    , description
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

description : Avatar -> String
description a =
    (Height.toString a.height) 
    ++ " and " 
    ++ (Build.toString a.build) 
    ++ " frame | "
    ++ (Complexion.toString a.complexion)
    ++ " complexion | "
    ++ (HairColor.toString a.hairColor) 
    ++ ", " 
    ++ (HairStyle.toString a.hairStyle)
    ++ " hair | "
    ++ (EyeColor.toString a.eyeColor)
    ++ " eyes"