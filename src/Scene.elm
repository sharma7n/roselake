module Scene exposing
    ( Scene(..)
    )

import Action exposing (Action)
import Battle exposing (Battle)
import DelvePhase exposing (DelvePhase)
import Delve exposing (Delve)
import Monster exposing (Monster)
import Reward exposing (Reward)
import Shop exposing (Shop)
import BossPhase exposing (BossPhase)
import BossState exposing (BossState)

type Scene
    = Player
    | Essentia
    | LearnSelect
    | Equip
    | Home
    | ShopSelect
    | Shop
    | Explore
    | ExploreDungeon
    | BattleSelect
    | BattleMonsterLoadingIntent
    | BattleMonster Action
    | VictoryLoading
    | Victory Reward
    | GameOver
    | Escaped
    | Town
    | OnyxTower
    | BossSelect
    | BossFight