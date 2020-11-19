module DelvePhase exposing
    ( DelvePhase(..)
    )

import DungeonPath
import DungeonScene

type DelvePhase
    = ExplorationPhase (List DungeonPath.Path)
    | ActionPhase DungeonScene.Scene