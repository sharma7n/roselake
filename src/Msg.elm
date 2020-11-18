module Msg exposing
    ( Msg(..)
    )

import CharacterCreationModel exposing (CharacterCreationModel)
import CharacterCreationSettingSelection exposing (CharacterCreationSettingSelection)

import Attribute exposing (Attribute)
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
import Boss exposing (Boss)
import BossPath exposing (BossPath)
import BossScene exposing (BossScene)
import Weapon exposing (Weapon)

import EssentiaContainer exposing (EssentiaContainer)

import Scene exposing (Scene)

type Msg
    = UserSelectedScene Scene
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
    | SystemGotMonsterTemplate MonsterTemplate
    | SystemGotObject Object
    | SystemGotReward Reward
    | SystemGotShop Shop
    | UserSelectedRest
    | UserSelectedOpenChest
    | UserSelectedOnyxTower
    -- CHARACTER CREATION
    | UserSelectedCharacterCreationSettingSelection CharacterCreationSettingSelection
    | UserSelectedModifyCharacterCreationAttribute Attribute Int
    | SystemGotCharacterCreationModel CharacterCreationModel
    | UserSelectedCharacterCreationConfirmation
    | UserSelectedRandomCharacterCreation
    | DevSelectedCharacterCreationConfirmation
    -- DUNGEON
    | UserSelectedExploreDungeonScene Dungeon
    | SystemGotDungeonInitialization Dungeon (List DungeonPath.Path)
    | UserSelectedDungeonPath DungeonPath.Path
    | SystemGotDungeonScene DungeonScene.Scene
    | UserSelectedContinueDungeon
    | SystemGotDungeonContinuation (List DungeonPath.Path)
    | UserSelectedExitDungeon
    -- BOSS
    | UserSelectedBossFight Boss
    | SystemGotBossInitialization Boss (List BossPath)
    | UserSelectedBossPath BossPath
    | SystemGotBossScene BossScene
    | UserSelectedContinueBossFight Boss
    | SystemGotBossFightContinuation (List BossPath)
    | SystemGotBossMonsterIntent Action
    -- BATTLE
    | SystemGotMonsterIntent Action
    | UserSelectedBattleAction Action
    | UserSelectedEndBattleTurn