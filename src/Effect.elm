module Effect exposing
    ( Effect(..)
    )

type Effect
    = ChangeLevel Int
    | ChangeExperience Int
    | ChangeSatiety Int
    | ChangeMaxSatiety Int
    | ChangeHitPoints Int
    | ChangeMaxHitPoints Int
    | ChangeMagicPoints Int
    | ChangeMaxMagicPoints Int
    | ChangeAttack Int