module Formula exposing
    ( Formula(..)
    )

type Formula
    = Attack
    | AxeAttack
    | BowAttack
    | ClawAttack
    | StaffAttack
    | Block
    | FireBreath
    | Fire Int
    | Heal Int
    | ChargeUp Int
    | Explode
    | Curse
    | Poison