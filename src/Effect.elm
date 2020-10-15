module Effect exposing
    ( Effect(..)
    , contexts
    )

import Context exposing (Context)

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

contexts : Effect -> List Context
contexts effect =
    case effect of
        ChangeLevel _ ->
            [ Context.None ]
        
        ChangeExperience _ ->
            [ Context.None ]
        
        ChangeSatiety _ ->
            [ Context.None ]
        
        ChangeMaxSatiety _ ->
            [ Context.None ]
        
        ChangeHitPoints _ ->
            [ Context.Any ]
        
        ChangeMaxHitPoints _ ->
            [ Context.None ]
        
        ChangeMagicPoints _ ->
            [ Context.Any ]
        
        ChangeMaxMagicPoints _ ->
            [ Context.None ]
        
        ChangeAttack _ ->
            [ Context.None ]