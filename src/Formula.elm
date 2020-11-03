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
    | MegaFlare
    | Fire Int
    | Heal Int
    | ChargeUp Int
    | Explode
    | Curse
    | Poison