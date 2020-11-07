module Msg exposing
    ( Msg(..)
    )

import CharacterCreationModel exposing (CharacterCreationModel)
import CharacterCreationSettingSelection exposing (CharacterCreationSettingSelection)

import Action exposing (Action)
import Passive exposing (Passive)
import Armor exposing (Armor)
import Dungeon exposing (Dungeon)
import DungeonPath
import DungeonScene
import Essentia exposing (Essentia)
import Item exposing (Item)
import MonsterTemplate exposing (MonsterTemplate)
import Object exposing (Object)
import Reward exposing (Reward)
import Shop exposing (Shop)
import Weapon exposing (Weapon)

import EssentiaContainer exposing (EssentiaContainer)

import Scene exposing (Scene)

type Msg
    = NoOp
    | UserSelectedScene Scene
    | UserSelectedMonsterTemplate MonsterTemplate
    | UserSelectedLearnSkill Action
    | UserSelectedLearnPassive Passive
    | UserSelectedHomeRest
    | UserSelectedShop Shop
    | UserSelectedBuy Item
    | UserSelectedUseItem Item
    | UserSelectedEquipWeapon Weapon
    | UserSelectedUnEquipWeapon Weapon
    | UserSelectedEquipArmor Armor
    | UserSelectedUnEquipArmor Armor
    | UserSelectedEquipEssentia EssentiaContainer.Index Int Essentia
    | UserSelectedUnEquipEssentia EssentiaContainer.Index Essentia
    | UserSelectedExploreDungeonScene Dungeon
    | SystemGotDungeonInitialization Dungeon (List DungeonPath.Path)
    | UserSelectedDungeonPath DungeonPath.Path
    | SystemGotDungeonScene DungeonScene.Scene
    | UserSelectedContinueDungeon
    | UserSelectedExitDungeon
    | SystemGotDungeonContinuation (List DungeonPath.Path)
    | SystemGotMonsterTemplate MonsterTemplate
    | SystemGotMonsterIntent Action
    | SystemGotObject Object
    | SystemGotReward Reward
    | SystemGotShop Shop
    | SystemGotCharacterCreationModel CharacterCreationModel
    | UserSelectedBattleAction Action
    | UserSelectedEndBattleTurn
    | UserSelectedRest
    | UserSelectedOpenChest
    | UserSelectedCharacterCreationSettingSelection CharacterCreationSettingSelection
    | UserSelectedCharacterCreationConfirmation
    | UserSelectedRandomCharacterCreation
    | DevSelectedCharacterCreationConfirmation