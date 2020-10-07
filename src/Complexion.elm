module Complexion exposing
    ( Complexion(..)
    , all
    , toString
    )

type Complexion
    = Pale
    | Fair
    | Medium
    | Olive
    | Brown
    | Black

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