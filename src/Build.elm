module Build exposing
    ( Build(..)
    , all
    , toString
    , generator
    )

import Random

import Util

type Build
    = Average
    | Lithe
    | Sturdy

generator : Random.Generator Build
generator =
    Util.uniformGenerator Average all

all : List Build
all =
    [ Average
    , Lithe
    , Sturdy
    ]

toString : Build -> String
toString x =
    case x of
        Average ->
            "Average"
        
        Lithe ->
            "Lithe"
        
        Sturdy ->
            "Sturdy"