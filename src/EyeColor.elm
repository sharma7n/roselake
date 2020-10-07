module EyeColor exposing
    ( EyeColor(..)
    , all
    , toString
    )

type EyeColor
    = Black
    | Brown
    | Blue

all : List EyeColor
all =
    [ Black
    , Brown
    , Blue
    ]

toString : EyeColor -> String
toString x =
    case x of
        Black ->
            "Black"
        
        Brown ->
            "Brown"
        
        Blue ->
            "Blue"