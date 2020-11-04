module HairStyle exposing
    ( HairStyle(..)
    , all
    , toString
    , generator
    )

import Random

import Util

type HairStyle
    = Plain

generator : Random.Generator HairStyle
generator =
    Util.uniformGenerator Plain all

all : List HairStyle
all =
    [ Plain
    ]

toString : HairStyle -> String
toString x =
    case x of
        Plain ->
            "Plain"