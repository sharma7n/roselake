module Height exposing
    ( Height(..)
    , all
    , toString
    )

type Height
    = Average
    | Short
    | Tall

all : List Height
all =
    [ Average
    , Short
    , Tall
    ]

toString : Height -> String
toString x =
    case x of
        Average ->
            "Average"
        
        Short ->
            "Short"
        
        Tall ->
            "Tall"