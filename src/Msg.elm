module Msg exposing
    ( Msg(..)
    )

import CharacterCreationSettingSelection exposing (CharacterCreationSettingSelection)

import Action exposing (Action)
import Armor exposing (Armor)
import Dungeon exposing (Dungeon)
import DungeonPath
import DungeonScene
import Item exposing (Item)
import Monster exposing (Monster)
import Object exposing (Object)
import Reward exposing (Reward)
import Shop exposing (Shop)
import Weapon exposing (Weapon)

import Scene exposing (Scene)

type Msg
    = NoOp
    | UserSelectedScene Scene
    | UserSelectedMonster Monster
    | UserSelectedLearnSkill Action
    | UserSelectedHomeRest
    | UserSelectedShop Shop
    | UserSelectedBuy Item
    | UserSelectedUseItem Item
    | UserSelectedEquipWeapon Weapon
    | UserSelectedUnEquipWeapon Weapon
    | UserSelectedEquipArmor Armor
    | UserSelectedUnEquipArmor Armor
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
    | UserSelectedBattleAction Action
    | UserSelectedEndBattleTurn
    | UserSelectedRest
    | UserSelectedOpenChest
    | UserSelectedCharacterCreationSettingSelection CharacterCreationSettingSelection
    | UserSelectedCharacterCreationConfirmation
    | DevSelectedCharacterCreationConfirmation