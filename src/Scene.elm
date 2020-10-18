module Scene exposing
    ( Scene(..)
    )

import Action exposing (Action)
import Delve exposing (Delve)
import DelvePhase exposing (DelvePhase)
import Monster exposing (Monster)
import Reward exposing (Reward)
import Shop exposing (Shop)

type Scene
    = PlayerScene
    | LearnSelectScene
    | HomeScene
    | ShopSelectScene
    | ShopScene Shop
    | ExploreScene
    | ExploreDungeonScene DelvePhase Delve
    | BattleScene
    | BattleMonsterLoadingIntentScene Monster
    | BattleMonsterScene Monster Action
    | VictoryScene Monster Reward
    | GameOverScene