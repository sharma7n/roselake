module DelvePhase exposing
    ( DelvePhase(..)
    )

import DungeonPath
import DungeonScene
import DungeonScenario exposing (DungeonScenario)

type DelvePhase
    = ExplorationPhase (List DungeonPath.Path)
    | DecisionPhase DungeonScenario (List DungeonChoice)
    | ActionPhase DungeonScene.Scene