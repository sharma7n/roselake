module Scene exposing
    ( Scene(..)
    )

import Action exposing (Action)
import DelvePhase exposing (DelvePhase)
import Reward exposing (Reward)
import Shop exposing (Shop)
import BossPhase exposing (BossPhase)

type Scene
    = Player
    | Essentia
    | LearnSelect
    | Equip
    | Home
    | ShopSelect
    | Shop Shop
    | DungeonSelect
    | Dungeon DelvePhase
    | BattleSelect
    | BattleLoadingIntent
    | Battle Action
    | VictoryLoading
    | Victory Reward
    | GameOver
    | Escaped
    | Town
    | OnyxTower
    | BossSelect