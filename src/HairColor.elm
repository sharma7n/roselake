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
    | Gray
    | White
    | Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Purple
    | Brown

generator : Random.Generator HairColor
generator =
    Util.uniformGenerator Black all

all : List HairColor
all =
    [ Black
    , Gray
    , White
    , Red
    , Orange
    , Yellow
    , Green
    , Blue
    , Purple
    , Brown
    ]

toString : HairColor -> String
toString x =
    case x of
        Black ->
            "Black"
        
        Gray ->
            "Gray"
        
        White ->
            "White"
        
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
        
        Brown ->
            "Brown"