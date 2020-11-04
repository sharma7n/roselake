module Complexion exposing
    ( Complexion(..)
    , all
    , toString
    , generator
    )

import Random

import Util

type Complexion
    = Pale
    | Fair
    | Medium
    | Olive
    | Brown
    | Black

generator : Random.Generator Complexion
generator =
    Util.uniformGenerator Pale all

all : List Complexion
all =
    [ Pale
    , Fair
    , Medium
    , Olive
    , Brown
    , Black
    ]

toString : Complexion -> String
toString x =
    case x of
        Pale ->
            "Pale"
        
        Fair ->
            "Fair"
        
        Medium ->
            "Medium"
        
        Olive ->
            "Olive"
        
        Brown ->
            "Brown"
        
        Black ->
            "Black"