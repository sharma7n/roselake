module Avatar exposing
    ( Avatar
    , render
    )

import HairStyle exposing (HairStyle)
import HairColor exposing (HairColor)
import EyeColor exposing (EyeColor)
import Complexion exposing (Complexion)
import Height exposing (Height)
import Build exposing (Build)

import Svg exposing (Svg)
import Svg.Attributes as A

type alias Avatar =
    { hairStyle : HairStyle
    , hairColor : HairColor
    , eyeColor : EyeColor
    , complexion : Complexion
    , height : Height
    , build : Build
    }

render : Avatar -> Svg a
render _ =
    Svg.image
        [ A.width "32"
        , A.height "64"
        , A.viewBox "0 0 32 64"
        ]
        [ Svg.circle
            [ A.cx "10"
            , A.cy "10"
            , A.r "10"
            , A.color "#000"
            ]
            []
        ]