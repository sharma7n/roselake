module SceneState exposing
    ( SceneState
    , Ambient(..)
    , setBattle
    , clearBattle
    , new
    )

import Action exposing (Action)
import Battle exposing (Battle)
import Delve exposing (Delve)
import DelvePhase exposing (DelvePhase)
import BossPhase exposing (BossPhase)
import BossState exposing (BossState)

type alias SceneState =
    { ambient : Ambient
    , maybeBattle : Maybe Battle
    , maybeMonsterAction : Maybe Action
    }

type Ambient
    = Rest
    | Delving DelvePhase Delve
    | BossFight BossPhase BossState

new : SceneState
new =
    { ambient = Rest
    , maybeBattle = Nothing
    , maybeMonsterAction = Nothing
    }

setBattle : Battle -> SceneState -> SceneState
setBattle battle s =
    { s | maybeBattle = Just battle }

clearBattle : SceneState -> SceneState
clearBattle s =
    { s | maybeBattle = Nothing }