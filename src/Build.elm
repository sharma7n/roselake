module Build exposing
    ( Build(..)
    , all
    , toString
    )

type Build
    = Average
    | Lithe
    | Sturdy

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