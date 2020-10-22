module Formula exposing
    ( Formula(..)
    )

type Formula
    = Attack
    | Block
    | FireBreath
    | Fire Int
    | Heal Int
    | ChargeUp Int