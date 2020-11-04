module HairColor exposing
    ( HairColor(..)
    , all
    , toString
    , generator
    )

import Random

import Util

type HairColor
    = Black
    | Brown
    | Blonde
    | Red

generator : Random.Generator HairColor
generator =
    Util.uniformGenerator Black all

all : List HairColor
all =
    [ Black
    , Brown
    , Blonde
    , Red
    ]

toString : HairColor -> String
toString x =
    case x of
        Black ->
            "Black"
        
        Brown ->
            "Brown"
        
        Blonde ->
            "Blonde"
        
        Red ->
            "Red"