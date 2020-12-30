module SceneState exposing
    ( SceneState
    , Ambient(..)
    , setBattle
    , clearBattle
    , new
    )

import Battle exposing (Battle)
import Delve exposing (Delve)
import BossState exposing (BossState)

type alias SceneState =
    { ambient : Ambient
    , maybeBattle : Maybe Battle
    }

type Ambient
    = Rest
    | Delving Delve
    | BossFight BossState

new : SceneState
new =
    { ambient = Rest
    , maybeBattle = Nothing
    }

setBattle : Battle -> SceneState -> SceneState
setBattle battle s =
    { s | maybeBattle = Just battle }

clearBattle : SceneState -> SceneState
clearBattle s =
    { s | maybeBattle = Nothing }