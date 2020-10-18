module Msg exposing
    ( Msg(..)
    )

import CharacterCreationSettingSelection exposing (CharacterCreationSettingSelection)

import Action exposing (Action)
import Dungeon exposing (Dungeon)
import DungeonPath
import DungeonScene
import Item exposing (Item)
import Monster exposing (Monster)
import Object exposing (Object)
import Reward exposing (Reward)
import Shop exposing (Shop)

type Msg
    = NoOp
    | UserSelectedPlayerScene
    | UserSelectedLearnSelectScene
    | UserSelectedLearnSkill Action
    | UserSelectedHomeScene
    | UserSelectedHomeRest
    | UserSelectedShopSelectScene
    | UserSelectedShop Shop
    | UserSelectedBuy Item
    | UserSelectedUseItem Item
    | UserSelectedExploreScene
    | UserSelectedExploreDungeonScene Dungeon
    | SystemGotDungeonInitialization Dungeon (List DungeonPath.Path)
    | UserSelectedDungeonPath DungeonPath.Path
    | SystemGotDungeonScene DungeonScene.Scene
    | UserSelectedContinueDungeon
    | UserSelectedExitDungeon
    | SystemGotDungeonContinuation (List DungeonPath.Path)
    | SystemGotMonster Monster
    | SystemGotMonsterIntent Action
    | SystemGotObject Object
    | SystemGotReward Reward
    | SystemGotShop Shop
    | UserSelectedBattleScene
    | UserSelectedBattleMonsterScene Monster
    | UserSelectedBattleAction Action
    | UserSelectedRest
    | UserSelectedOpenChest
    | UserSelectedCharacterCreationSettingSelection CharacterCreationSettingSelection
    | UserSelectedCharacterCreationConfirmation
    | DevSelectedCharacterCreationConfirmation