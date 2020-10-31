module WeaponKind exposing
    ( WeaponKind(..)
    , all
    , toString
    , attackAction
    )

import Action exposing (Action)

type WeaponKind
    = Axe
    | Bow
    | Claw
    | Staff

fold : a -> a -> a -> a -> WeaponKind -> a
fold axe bow claw staff w =
    case w of
        Axe ->
            axe
        
        Bow ->
            bow
        
        Claw ->
            claw

        Staff ->
            staff

all : List WeaponKind
all =
    [ Axe
    , Bow
    , Claw
    , Staff
    ]

toString : WeaponKind -> String
toString =
    fold "Axe" "Bow" "Claw" "Staff"

attackAction : WeaponKind -> Action
attackAction =
    fold 
        (Action.byId "axe-attack")
        (Action.byId "bow-attack")
        (Action.byId "claw-attack")
        (Action.byId "staff-attack")