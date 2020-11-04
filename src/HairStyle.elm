module HairStyle exposing
    ( HairStyle(..)
    , all
    , toString
    , generator
    )

import Random

import Util

type HairStyle
    = Bald
    | Curly
    | Spiky
    | Short
    | Long
    | Ponytail

generator : Random.Generator HairStyle
generator =
    Util.uniformGenerator Bald all

all : List HairStyle
all =
    [ Bald
    , Curly
    , Spiky
    , Short
    , Long
    , Ponytail
    ]

toString : HairStyle -> String
toString x =
    case x of
        Bald ->
            "Bald"
        
        Curly ->
            "Curly"
        
        Spiky ->
            "Spiky"
        
        Short ->
            "Short"
        
        Long ->
            "Long"
        
        Ponytail ->
            "Ponytail"