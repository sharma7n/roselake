module SceneState exposing
    ( SceneState
    , Ambient(..)
    , setBattle
    , clearBattle
    , setMonsterAction
    , setShop
    , setReward
    , clearReward
    , new
    )

import Action exposing (Action)
import Battle exposing (Battle)
import Delve exposing (Delve)
import DelvePhase exposing (DelvePhase)
import Shop exposing (Shop)
import Reward exposing (Reward)

type alias SceneState =
    { ambient : Ambient
    , maybeBattle : Maybe Battle
    , maybeMonsterAction : Maybe Action
    , maybeShop : Maybe Shop
    , maybeReward : Maybe Reward
    }

type Ambient
    = Rest
    | Delving DelvePhase Delve

new : Ambient -> SceneState
new ambient =
    { ambient = ambient
    , maybeBattle = Nothing
    , maybeMonsterAction = Nothing
    , maybeShop = Nothing
    , maybeReward = Nothing
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

setReward : Reward -> SceneState -> SceneState
setReward reward s =
    { s | maybeReward = Just reward }

clearReward : SceneState -> SceneState
clearReward s =
    { s | maybeReward = Nothing}