module SceneState exposing
    ( SceneState
    , Ambient(..)
    , setBattle
    , clearBattle
    , setMonsterAction
    , setShop
    , new
    )

import Action exposing (Action)
import Battle exposing (Battle)
import Delve exposing (Delve)
import DelvePhase exposing (DelvePhase)
import BossPhase exposing (BossPhase)
import BossState exposing (BossState)
import Shop exposing (Shop)

type alias SceneState =
    { ambient : Ambient
    , maybeBattle : Maybe Battle
    , maybeMonsterAction : Maybe Action
    , maybeShop : Maybe Shop
    }

type Ambient
    = Rest
    | Delving DelvePhase Delve
    | BossFight BossPhase BossState

new : Ambient -> SceneState
new ambient =
    { ambient = ambient
    , maybeBattle = Nothing
    , maybeMonsterAction = Nothing
    , maybeShop = Nothing
    }

setBattle : Battle -> SceneState -> SceneState
setBattle battle s =
    { s | maybeBattle = Just battle }

clearBattle : SceneState -> SceneState
clearBattle s =
    { s | maybeBattle = Nothing }

setMonsterAction : Action -> SceneState -> SceneState
setMonsterAction monsterAction s =
    { s | maybeMonsterAction = Just monsterAction }

setShop : Shop -> SceneState -> SceneState
setShop shop s =
    { s | maybeShop = Just shop }