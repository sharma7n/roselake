module EyeColor exposing
    ( EyeColor(..)
    , all
    , toString
    , generator
    )

import Random

import Util

type EyeColor
    = Black
    | Brown
    | Blue

generator : Random.Generator EyeColor
generator =
    Util.uniformGenerator Black all

all : List EyeColor
all =
    [ Black
    , Brown
    , Blue
    ]

toString : EyeColor -> String
toString x =
    case x of
        Black ->
            "Black"
        
        Brown ->
            "Brown"
        
        Blue ->
            "Blue"