module BossPhase exposing
    ( BossPhase(..)
    )

import BossPath
import BossScene

type BossPhase
    = ExplorationPhase (List BossPath.Path)
    | ActionPhase BossScene.Scene