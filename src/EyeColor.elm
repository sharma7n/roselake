module EyeColor exposing
    ( EyeColor(..)
    , all
    , toString
    , generator
    )

import Random

import Util

type EyeColor
    = Brown
    | Gray
    | Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Purple

generator : Random.Generator EyeColor
generator =
    Util.uniformGenerator Brown all

all : List EyeColor
all =
    [ Brown
    , Gray
    , Red
    , Orange
    , Yellow
    , Green
    , Blue
    , Purple
    ]

toString : EyeColor -> String
toString x =
    case x of
        Brown ->
            "Brown"
        
        Gray ->
            "Gray"
        
        Red ->
            "Red"
        
        Orange ->
            "Orange"
        
        Yellow ->
            "Yellow"
        
        Green ->
            "Green"
        
        Blue ->
            "Blue"
        
        Purple ->
            "Purple"