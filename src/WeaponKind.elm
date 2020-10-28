module WeaponKind exposing
    ( WeaponKind(..)
    , all
    , toString
    )

type WeaponKind
    = Axe
    | Claw
    | Bow
    | Staff

fold : a -> a -> a -> a -> WeaponKind -> a
fold axe claw bow staff w =
    case w of
        Axe ->
            axe
        
        Claw ->
            claw

        Bow ->
            bow

        Staff ->
            staff

all : List WeaponKind
all =
    [ Axe
    , Claw
    , Bow
    , Staff
    ]

toString : WeaponKind -> String
toString =
    fold "Axe" "Blow" "Claw" "Staff"