module BossPhase exposing
    ( BossPhase(..)
    )

import BossPath exposing (BossPath)
import BossScene exposing (BossScene)

type BossPhase
    = ExplorationPhase (List BossPath)
    | ActionPhase BossScene