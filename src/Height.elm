module Height exposing
    ( Height(..)
    , all
    , toString
    , generator
    )

import Random

import Util

type Height
    = Average
    | Short
    | Tall

generator : Random.Generator Height
generator =
    Util.uniformGenerator Average all

all : List Height
all =
    [ Average
    , Short
    , Tall
    ]

toString : Height -> String
toString x =
    case x of
        Average ->
            "Average"
        
        Short ->
            "Short"
        
        Tall ->
            "Tall"