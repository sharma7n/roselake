module HairColor exposing
    ( HairColor(..)
    , all
    , toString
    )

type HairColor
    = Black
    | Brown
    | Blonde
    | Red

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