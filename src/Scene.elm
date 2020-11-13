module Scene exposing
    ( Scene(..)
    )

import Action exposing (Action)
import Battle exposing (Battle)
import Delve exposing (Delve)
import DelvePhase exposing (DelvePhase)
import Monster exposing (Monster)
import Reward exposing (Reward)
import Shop exposing (Shop)
import BossPhase exposing (BossPhase)
import BossState exposing (BossState)

type Scene
    = PlayerScene
    | EssentiaScene
    | LearnSelectScene
    | EquipScene
    | HomeScene
    | ShopSelectScene
    | ShopScene Shop
    | ExploreScene
    | ExploreDungeonScene DelvePhase Delve
    | BattleScene
    | BattleMonsterLoadingIntentScene Battle
    | BattleMonsterScene Battle Action
    | VictoryLoadingScene Battle
    | VictoryScene Battle Reward
    | GameOverScene
    | EscapedScene
    | TownScene
    | OnyxTowerScene
    | BossSelectScene
    | BossFightScene BossPhase BossState