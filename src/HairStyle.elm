module HairStyle exposing
    ( HairStyle(..)
    , all
    , toString
    )

type HairStyle
    = Plain

all : List HairStyle
all =
    [ Plain
    ]

toString : HairStyle -> String
toString x =
    case x of
        Plain ->
            "Plain"