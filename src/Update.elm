module Update exposing
    ( update
    )

import Random
import Set exposing (Set)

import Distribution exposing (Distribution)
import Util

import CharacterCreationError
import CharacterCreationModel exposing (CharacterCreationModel)
import CharacterCreationSettings exposing (CharacterCreationSettings)
import CharacterCreationSettingSelection exposing (CharacterCreationSettingSelection)

import FormResult exposing (FormResult)
import HairStyle exposing (HairStyle)
import HairColor exposing (HairColor)
import EyeColor exposing (EyeColor)
import Complexion exposing (Complexion)
import Height exposing (Height)
import Build exposing (Build)

import Attribute exposing (Attribute)
import Armor exposing (Armor)
import Battle exposing (Battle)
import Battler exposing (Battler)
import Target exposing (Target)
import Avatar exposing (Avatar)
import Delve exposing (Delve)
import DelvePhase exposing (DelvePhase)
import DungeonPath
import DungeonScene
import Dungeon exposing (Dungeon)
import Essentia exposing (Essentia)
import Action exposing (Action)
import Passive exposing (Passive)
import ActionState exposing (ActionState)
import Effect exposing (Effect)
import Monster exposing (Monster)
import Reward exposing (Reward)
import Inventory exposing (Inventory)
import Object exposing (Object)
import Shop exposing (Shop)
import Item exposing (Item)
import Weapon exposing (Weapon)
import Status exposing (Status)
import Battle exposing (Battle)
import BossPath exposing (BossPath)
import BossPhase exposing (BossPhase)
import BossState exposing (BossState)
import BossScene exposing (BossScene)

import EssentiaContainer exposing (EssentiaContainer)

import Scene exposing (Scene)
import SceneModel exposing (SceneModel)

import Phase exposing (Phase)

import Model exposing (Model)
import Msg exposing (Msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.phase ) of        
        ( Msg.UserSelectedScene scene, Phase.ScenePhase _ sceneModel ) ->
            ( { model | phase = Phase.ScenePhase scene sceneModel }, Cmd.none )
        
        ( Msg.UserSelectedMonsterTemplate monsterTemplate, Phase.ScenePhase _ sceneModel ) ->
            let
                battle =
                    Battle.new (Monster.new monsterTemplate)
                
                cmd =
                    Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction battle)
                
                newSceneModel =
                    SceneModel.completeBattle sceneModel
            in
            ( { model | phase = Phase.ScenePhase (Scene.BattleMonsterLoadingIntentScene battle) newSceneModel }, cmd )
        
        ( Msg.UserSelectedLearnSkill action, Phase.ScenePhase scene sceneModel ) ->
            let
                newSceneModel =
                    if action.learnCost <= sceneModel.freeAbilityPoints then
                        { sceneModel
                            | freeAbilityPoints =
                                sceneModel.freeAbilityPoints - action.learnCost
                            , actions =
                                action :: sceneModel.actions
                            , learned =
                                sceneModel.learned
                                    |> Set.insert action.id
                        }
                    else
                        sceneModel
            in
            ( { model | phase = Phase.ScenePhase scene newSceneModel }, Cmd.none )
        
        ( Msg.UserSelectedLearnPassive passive, Phase.ScenePhase scene sceneModel ) ->
            let
                newSceneModel =
                    if passive.learnCost <= sceneModel.freeAbilityPoints then
                        { sceneModel
                            | freeAbilityPoints =
                                sceneModel.freeAbilityPoints - passive.learnCost
                            , passives =
                                passive :: sceneModel.passives
                            , learnedPassives =
                                sceneModel.learnedPassives
                                    |> Set.insert passive.id
                        }
                    else
                        sceneModel
            in
            ( { model | phase = Phase.ScenePhase scene newSceneModel }, Cmd.none )
        
        ( Msg.UserSelectedEquipWeapon weapon, Phase.ScenePhase scene sceneModel ) ->
            let
                newInventory =
                    case sceneModel.equippedWeapon of
                        Just heldWeapon ->
                            sceneModel.inventory
                                |> Inventory.modifyWeaponQuantity weapon -1
                                |> Inventory.modifyWeaponQuantity heldWeapon 1
                        
                        Nothing ->
                            sceneModel.inventory
                                |> Inventory.modifyWeaponQuantity weapon -1
                
                newSceneModel =
                    { sceneModel
                        | inventory = newInventory
                        , equippedWeapon = Just weapon
                    }
                
                newModel =
                    { model | phase = Phase.ScenePhase scene newSceneModel }
                
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedUnEquipWeapon weapon, Phase.ScenePhase scene sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel
                        | equippedWeapon = Nothing
                        , inventory =
                            sceneModel.inventory
                                |> Inventory.modifyWeaponQuantity weapon 1
                    }
                newModel =
                    { model | phase = Phase.ScenePhase scene newSceneModel }
                
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedEquipArmor armor, Phase.ScenePhase scene sceneModel ) ->
            let
                newInventory =
                    case sceneModel.equippedArmor of
                        Just heldArmor ->
                            sceneModel.inventory
                                |> Inventory.modifyArmorQuantity armor -1
                                |> Inventory.modifyArmorQuantity heldArmor 1
                        
                        Nothing ->
                            sceneModel.inventory
                                |> Inventory.modifyArmorQuantity armor -1
                
                newSceneModel =
                    { sceneModel
                        | inventory = newInventory
                        , equippedArmor = Just armor
                    }
                
                newModel =
                    { model | phase = Phase.ScenePhase scene newSceneModel }
                
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedUnEquipArmor armor, Phase.ScenePhase scene sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel
                        | equippedArmor = Nothing
                        , inventory =
                            sceneModel.inventory
                                |> Inventory.modifyArmorQuantity armor 1
                    }
                newModel =
                    { model | phase = Phase.ScenePhase scene newSceneModel }
                
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedEquipEssentia idx listIdx essentia, Phase.ScenePhase scene sceneModel ) ->
            let
                equippedEssentia =
                    EssentiaContainer.getSlot idx sceneModel.essentiaContainer
                
                newEssentia =
                    sceneModel.essentia
                        |> Util.removeListAt listIdx
                        |> Util.appendMaybe equippedEssentia
                
                newEssentiaContainer =
                    sceneModel.essentiaContainer
                        |> EssentiaContainer.setSlot idx essentia
                
                newSceneModel =
                    { sceneModel
                        | essentia = newEssentia
                        , essentiaContainer = newEssentiaContainer
                    }
                
                newModel =
                    { model | phase = Phase.ScenePhase scene newSceneModel }
                
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedUnEquipEssentia idx essentia, Phase.ScenePhase scene sceneModel ) ->
            let
                equippedEssentia =
                    EssentiaContainer.getSlot idx sceneModel.essentiaContainer
                
                newEssentia =
                    sceneModel.essentia
                        |> Util.appendMaybe equippedEssentia
                
                newEssentiaContainer =
                    sceneModel.essentiaContainer
                        |> EssentiaContainer.clearSlot idx
                
                newSceneModel =
                    { sceneModel
                        | essentia = newEssentia
                        , essentiaContainer = newEssentiaContainer
                    }
                
                newModel =
                    { model | phase = Phase.ScenePhase scene newSceneModel }
                
            in
            ( newModel, Cmd.none )
           
        ( Msg.UserSelectedHomeRest, Phase.ScenePhase scene sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel 
                        | hitPoints = sceneModel.maxHitPoints
                        , magicPoints = sceneModel.maxMagicPoints
                    }
            in
            ( { model | phase = Phase.ScenePhase scene newSceneModel }, Cmd.none )
               
        ( Msg.UserSelectedShop shop, Phase.ScenePhase Scene.ShopSelectScene sceneModel ) ->
            ( { model | phase = Phase.ScenePhase (Scene.ShopScene shop) sceneModel }, Cmd.none )
        
        ( Msg.UserSelectedBuy item, Phase.ScenePhase scene sceneModel ) ->
            let
                newSceneModel =
                    if item.cost <= sceneModel.gold then
                        { sceneModel
                            | gold = max 0 (sceneModel.gold - item.cost)
                            , inventory =
                                sceneModel.inventory
                                    |> Inventory.modifyItemQuantity item 1
                        }
                    else
                        sceneModel

            in
            ( { model | phase = Phase.ScenePhase scene newSceneModel }, Cmd.none )
        
        ( Msg.UserSelectedUseItem item, Phase.ScenePhase scene sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel
                        | inventory =
                            sceneModel.inventory
                                |> Inventory.modifyItemQuantity item -1
                    }
                        |> SceneModel.applyEffectsToSceneModel item.effects
            in
            ( { model | phase = Phase.ScenePhase scene newSceneModel }, Cmd.none )
        
        ( Msg.UserSelectedExploreDungeonScene dungeon, Phase.ScenePhase Scene.ExploreScene _ ) ->
            let
                pathListGenerator =
                    Random.list 3 DungeonPath.generator
                
                cmd =
                    Random.generate (Msg.SystemGotDungeonInitialization dungeon) pathListGenerator
            in
            ( model, cmd )
        
        ( Msg.UserSelectedBossFight boss, Phase.ScenePhase _ _ ) ->
            let
                pathListGenerator =
                    Random.list 3 ( BossPath.generator boss.bossBehavior )
                
                cmd =
                    Random.generate (Msg.SystemGotBossInitialization boss) pathListGenerator
            in
            ( model, cmd )
        
        ( Msg.SystemGotDungeonInitialization dungeon paths, Phase.ScenePhase Scene.ExploreScene sceneModel ) ->
            let
                delve =
                    { dungeon = dungeon
                    , floor = 1
                    }
            in
            ( { model | phase = Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ExplorationPhase paths) delve) sceneModel }, Cmd.none )
        
        ( Msg.SystemGotBossInitialization boss paths, Phase.ScenePhase _ sceneModel ) ->
            let
                bossState =
                    { boss = boss
                    , monster = Monster.new boss.monsterTemplate
                    }
                
                newPhase =
                    Phase.ScenePhase ( Scene.BossFightScene ( BossPhase.ExplorationPhase paths ) bossState ) sceneModel
                
                newModel =
                    { model
                        | phase = newPhase
                    }
            in
            ( newModel, Cmd.none )
            
        
        ( Msg.UserSelectedDungeonPath path, Phase.ScenePhase (Scene.ExploreDungeonScene _ _) _) ->
            let
                cmd =
                    Random.generate Msg.SystemGotDungeonScene (Distribution.random path.sceneDistribution)
            in
            ( model, cmd )
        
        ( Msg.UserSelectedBossPath path, Phase.ScenePhase _ _) ->
            let
                cmd =
                    Random.generate Msg.SystemGotBossScene (Distribution.random path.sceneDistribution)
            in
            ( model, cmd )
        
        ( Msg.SystemGotDungeonScene scene, Phase.ScenePhase (Scene.ExploreDungeonScene delvePhase delve) sceneModel ) ->
            let
                cmd =
                    case scene of
                        DungeonScene.Battle ->
                            Random.generate Msg.SystemGotMonsterTemplate (Dungeon.generateMonsterTemplate delve.dungeon)
                        
                        DungeonScene.Shop ->
                            Random.generate Msg.SystemGotShop Shop.generator
                        
                        _ ->
                            Cmd.none
                
            in
            ( { model | phase = Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase scene) delve) sceneModel }, cmd )
        
        ( Msg.SystemGotBossScene scene, Phase.ScenePhase (Scene.BossFightScene _ state) _ ) ->
            let
                newBattle =
                    Battle.new state.monster

                cmd =
                    case scene of
                        BossScene.BattleBoss ->
                            let
                                behaviorGenerator =
                                    Battle.chooseMonsterAction newBattle
                            in
                            Random.generate Msg.SystemGotBossMonsterIntent behaviorGenerator
                        
                        _ ->
                            Cmd.none                
            in
            ( model, cmd )
        
        ( Msg.UserSelectedOpenChest, Phase.ScenePhase (Scene.ExploreDungeonScene _ _) _ ) ->
            ( model, Random.generate Msg.SystemGotObject Object.generator )
        
        ( Msg.SystemGotShop shop, Phase.ScenePhase (Scene.ExploreDungeonScene _ delve) sceneModel ) ->
            ( { model | phase = Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.Shopping shop)) delve) sceneModel }, Cmd.none )
        
        ( Msg.SystemGotMonsterTemplate monsterTemplate, Phase.ScenePhase (Scene.ExploreDungeonScene delvePhase delve) sceneModel ) ->
            let
                battle = Battle.new (Monster.new monsterTemplate)
                
                cmd =
                    Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction battle)
                
                newSceneModel =
                    SceneModel.completeBattle sceneModel
            in
            ( { model | phase = Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.BattleMonsterLoadingIntent battle)) delve) newSceneModel }, cmd )
        
        ( Msg.SystemGotMonsterIntent intent, Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.BattleMonsterLoadingIntent battle)) delve) sceneModel ) ->
            ( { model | phase = Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.BattleMonster battle intent)) delve) sceneModel }, Cmd.none )

        ( Msg.SystemGotMonsterIntent intent, Phase.ScenePhase (Scene.BossFightScene (BossPhase.ActionPhase (BossScene.BattleBossLoadingIntent battle)) state) sceneModel ) ->
            ( { model | phase = Phase.ScenePhase (Scene.BossFightScene (BossPhase.ActionPhase (BossScene.BattleBossOngoing battle intent)) state) sceneModel }, Cmd.none )

        ( Msg.SystemGotBossMonsterIntent intent, Phase.ScenePhase (Scene.BossFightScene _ state ) sceneModel ) ->
            let
                newBattle =
                    Battle.new state.monster
                
                newBossPhase =
                    BossPhase.ActionPhase ( BossScene.BattleBossOngoing newBattle intent )
                
                newModel =
                    { model | phase = Phase.ScenePhase (Scene.BossFightScene newBossPhase state ) sceneModel }
            in
            ( newModel, Cmd.none )
        
        ( Msg.SystemGotObject object, Phase.ScenePhase (Scene.ExploreDungeonScene _ delve) sceneModel ) ->
            let
                newDungeonScene =
                    DelvePhase.ActionPhase <| DungeonScene.ReceiveTreasure object
                
                newInventory =
                    case object of
                        Object.Item i ->
                            sceneModel.inventory
                                |> Inventory.modifyItemQuantity i 1
                        
                        Object.Weapon w ->
                            sceneModel.inventory
                                |> Inventory.modifyWeaponQuantity w 1
                        
                        Object.Armor a ->
                            sceneModel.inventory
                                |> Inventory.modifyArmorQuantity a 1
                
                newSceneModel =
                    { sceneModel
                        | inventory = newInventory
                    }
                
                newModel =
                    { model
                        | phase = Phase.ScenePhase (Scene.ExploreDungeonScene newDungeonScene delve) newSceneModel
                    }
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedContinueDungeon, Phase.ScenePhase (Scene.ExploreDungeonScene _ _) _) ->
            let
                pathListGenerator =
                    Random.list 3 DungeonPath.generator

                cmd =
                    Random.generate Msg.SystemGotDungeonContinuation pathListGenerator
            in
            ( model, cmd )
        
        ( Msg.SystemGotDungeonContinuation paths, Phase.ScenePhase (Scene.ExploreDungeonScene _ delve) sceneModel ) ->
            let
                cmd =
                    if delve.floor >= delve.dungeon.depth then
                        Random.generate Msg.SystemGotReward (Dungeon.generateReward delve.dungeon)
                    else
                        Cmd.none
                
                newScene =
                    if delve.floor >= delve.dungeon.depth then
                        Scene.ExploreDungeonScene (DelvePhase.ActionPhase DungeonScene.LoadingGoal) delve
                    else
                        let
                            newDelve =
                                { delve
                                    | floor =
                                        delve.floor + 1
                                            |> Util.boundedBy 1 delve.dungeon.depth
                                }
                        in
                        Scene.ExploreDungeonScene (DelvePhase.ExplorationPhase paths) newDelve
            in 
            ( { model | phase = Phase.ScenePhase newScene sceneModel }, cmd )
        
        ( Msg.SystemGotReward reward, Phase.ScenePhase (Scene.VictoryLoadingScene monster) sceneModel ) ->
            let
                newSceneModel =
                    sceneModel
                        |> SceneModel.applyReward reward
                        |> SceneModel.completeBattle
            in
            ( { model | phase = Phase.ScenePhase (Scene.VictoryScene monster reward) newSceneModel }, Cmd.none )
        
        ( Msg.SystemGotReward reward, Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.VictoryLoading newMonster)) delve) sceneModel ) ->
            let
                newSceneModel =
                    sceneModel
                        |> SceneModel.applyReward reward
                        |> SceneModel.completeBattle
            in
            ( { model | phase = Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.Victory newMonster reward)) delve) newSceneModel }, Cmd.none )
        
        ( Msg.SystemGotReward reward, Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase DungeonScene.LoadingGoal) delve) sceneModel ) ->
            let
                newSceneModel =
                    sceneModel
                        |> SceneModel.applyReward reward
                        |> SceneModel.completeBattle
                        |> Util.forEach reward.items (\(item, qty) -> \s ->
                            { s | inventory = Inventory.modifyItemQuantity item qty s.inventory }
                        )
                
                newModel =
                    { model
                        | phase = Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.Goal reward)) delve) newSceneModel
                    }
            in
            ( newModel, Cmd.none )
        
        ( Msg.SystemGotMonsterIntent intent, Phase.ScenePhase (Scene.BattleMonsterLoadingIntentScene monster) sceneModel ) ->
            ( { model | phase = Phase.ScenePhase (Scene.BattleMonsterScene monster intent) sceneModel }, Cmd.none )
        
        ( Msg.UserSelectedBattleAction action, Phase.ScenePhase (Scene.BattleMonsterScene battle monsterAction) sceneModel ) ->
            updateBattleAction model battle action monsterAction sceneModel
        
        ( Msg.UserSelectedEndBattleTurn, Phase.ScenePhase (Scene.BattleMonsterScene battle monsterAction) sceneModel ) ->
            updateEndBattleTurn model battle monsterAction sceneModel
        
        ( Msg.UserSelectedBattleAction action, Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.BattleMonster battle monsterAction)) delve) sceneModel ) ->
            updateDungeonBattleAction model battle action monsterAction delve sceneModel
        
        ( Msg.UserSelectedEndBattleTurn, Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.BattleMonster battle monsterAction)) delve) sceneModel ) ->
            updateDungeonEndBattleTurn model battle monsterAction delve sceneModel
        
        ( Msg.UserSelectedBattleAction action, Phase.ScenePhase (Scene.BossFightScene (BossPhase.ActionPhase (BossScene.BattleBossOngoing battle monsterAction)) state) sceneModel ) ->
            updateBossBattleAction model battle action monsterAction state sceneModel
        
        ( Msg.UserSelectedEndBattleTurn, Phase.ScenePhase (Scene.BossFightScene (BossPhase.ActionPhase (BossScene.BattleBossOngoing battle monsterAction)) state) sceneModel ) ->
            updateBossEndBattleTurn model battle monsterAction state sceneModel
        
        ( Msg.UserSelectedRest, Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase DungeonScene.RestArea) delve) sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel
                        | hitPoints = sceneModel.maxHitPoints
                        , magicPoints = sceneModel.maxMagicPoints
                    }
                
                newModel =
                    { model
                        | phase = Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase DungeonScene.Rested) delve) newSceneModel
                    }
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedExitDungeon, Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase _) _) sceneModel ) ->
            ( { model | phase = Phase.ScenePhase Scene.ExploreScene sceneModel }, Cmd.none )
        
        ( Msg.UserSelectedCharacterCreationSettingSelection selection, Phase.CharacterCreationPhase characterCreationModel ) ->
            updateCharacterCreationSettingSelection model characterCreationModel selection
        
        ( Msg.UserSelectedModifyCharacterCreationAttribute attr d, Phase.CharacterCreationPhase characterCreationModel ) ->
            let
                c =
                    characterCreationModel
                
                newCharacterCreationModel =
                    if d >= 0 && c.attributePoints > 0 && CharacterCreationModel.getAttribute attr c < 9 then
                        c
                            |> CharacterCreationModel.modifyAttribute attr d
                            |> CharacterCreationModel.modifyAttributePoints (-1 * d)
                    else if d < 0 && c.attributePoints < 25 && CharacterCreationModel.getAttribute attr c > 1 then
                        c
                            |> CharacterCreationModel.modifyAttribute attr d
                            |> CharacterCreationModel.modifyAttributePoints (-1 * d)
                    else
                        c
                
                newModel =
                    { model
                        | phase = Phase.CharacterCreationPhase newCharacterCreationModel
                    }
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedCharacterCreationConfirmation, Phase.CharacterCreationPhase characterCreationModel ) ->
            updateCharacterCreationConfirmation model characterCreationModel
        
        ( Msg.UserSelectedRandomCharacterCreation, Phase.CharacterCreationPhase _ ) ->
            ( model, Random.generate Msg.SystemGotCharacterCreationModel CharacterCreationModel.generator )
        
        ( Msg.SystemGotCharacterCreationModel characterCreationModel, Phase.CharacterCreationPhase _ ) ->
            let
                newModel =
                    { model | phase = Phase.CharacterCreationPhase characterCreationModel }
            in
            ( newModel, Cmd.none )
        
        ( Msg.DevSelectedCharacterCreationConfirmation, Phase.CharacterCreationPhase characterCreationModel ) ->
            updateDevCharacterCreationConfirmation model characterCreationModel
        
        ( Msg.UserSelectedOnyxTower, Phase.ScenePhase _ sceneModel ) ->
            ( { model | phase = Phase.ScenePhase Scene.OnyxTowerScene sceneModel }, Cmd.none )
        
        _ ->
            ( model, Cmd.none )

updateCharacterCreationSettingSelection : Model -> CharacterCreationModel -> CharacterCreationSettingSelection -> ( Model, Cmd Msg )
updateCharacterCreationSettingSelection model characterCreationModel selection =
    let
        settings =
            characterCreationModel.settings
        
        newSettings =
            case selection of
                CharacterCreationSettingSelection.NameSelection name ->
                    { settings | name = FormResult.FROk name }
                
                CharacterCreationSettingSelection.HairStyleSelection hairStyle ->
                    { settings | hairStyle = FormResult.FROk hairStyle }
                
                CharacterCreationSettingSelection.HairColorSelection hairColor ->
                    { settings | hairColor = FormResult.FROk hairColor }
                
                CharacterCreationSettingSelection.EyeColorSelection eyeColor ->
                    { settings | eyeColor = FormResult.FROk eyeColor }
                
                CharacterCreationSettingSelection.ComplexionSelection complexion ->
                    { settings | complexion = FormResult.FROk complexion }
                
                CharacterCreationSettingSelection.HeightSelection height ->
                    { settings | height = FormResult.FROk height }
                
                CharacterCreationSettingSelection.BuildSelection build ->
                    { settings | build = FormResult.FROk build }
                
                CharacterCreationSettingSelection.StartingWeaponSelection startingWeapon ->
                    { settings | startingWeapon = FormResult.FROk startingWeapon }
                
                CharacterCreationSettingSelection.StartingEssentiaSelection startingEssentia ->
                    { settings | startingEssentia = FormResult.FROk startingEssentia }
    
        newCharacterCreationModel =
            { characterCreationModel | settings = newSettings }
        
        newModel =
            { model | phase = Phase.CharacterCreationPhase newCharacterCreationModel }
    in
    ( newModel, Cmd.none )

updateCharacterCreationConfirmation : Model -> CharacterCreationModel -> ( Model, Cmd Msg )
updateCharacterCreationConfirmation model characterCreationModel =
    let
        newSettings =
            CharacterCreationSettings.check characterCreationModel.settings
        
        sceneModelResult =
            SceneModel.characterCreationModelToSceneModel { characterCreationModel | settings = newSettings }
        
        newModel =
            case sceneModelResult of
                Ok sceneModel ->
                    { model 
                        | phase = Phase.ScenePhase Scene.PlayerScene sceneModel 
                    }
                
                Err _ ->
                    { model | phase = Phase.CharacterCreationPhase { characterCreationModel | settings = newSettings }}

    in
    ( newModel, Cmd.none )

updateDevCharacterCreationConfirmation : Model -> CharacterCreationModel -> ( Model, Cmd Msg )
updateDevCharacterCreationConfirmation model characterCreationModel =
    let
        newSettings =
            CharacterCreationSettings.check characterCreationModel.settings
        
        sceneModelResult =
            SceneModel.characterCreationModelToSceneModel { characterCreationModel | settings = newSettings }
        
        newModel =
            case sceneModelResult of
                Ok sceneModel ->
                    let
                        devSceneModel =
                            { sceneModel
                                | freeAbilityPoints = 100
                                , totalAbilityPoints = 100
                            }
                    in
                    { model | phase = Phase.ScenePhase Scene.PlayerScene devSceneModel }
                
                Err _ ->
                    { model | phase = Phase.CharacterCreationPhase { characterCreationModel | settings = newSettings }}

    in
    ( newModel, Cmd.none )

updateBattleAction : Model -> Battle -> Action -> Action -> SceneModel -> ( Model, Cmd Msg )
updateBattleAction model battle action monsterAction sceneModel =
    let
        ( newBattle, newSceneModel ) =
            ( battle, sceneModel )
                |> Battle.runPlayerAction action
        
        newMonster =
            newBattle.monster
        
        ( newScene, newSceneModel2, newCmd ) =
            if newBattle.state == Battle.Done then
                ( Scene.EscapedScene, SceneModel.completeBattle newSceneModel, Cmd.none )
            else if newSceneModel.hitPoints <= 0 then
                ( Scene.GameOverScene, SceneModel.completeBattle newSceneModel, Cmd.none )
            else if newMonster.hitPoints <= 0 then
                ( Scene.VictoryLoadingScene newBattle, newSceneModel, Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster) )
            else
                ( Scene.BattleMonsterScene newBattle monsterAction, newSceneModel, Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle) )
        
        newSceneModel3 =
            { newSceneModel2
                | actionStates =
                    newSceneModel2.actionStates
                        |> ActionState.performOneAction action
            }
    in
    ( { model | phase = Phase.ScenePhase newScene newSceneModel3 }, newCmd )

updateDungeonBattleAction : Model -> Battle -> Action -> Action -> Delve -> SceneModel -> ( Model, Cmd Msg )
updateDungeonBattleAction model battle action monsterAction delve sceneModel =
    let
        ( newBattle, newSceneModel ) =
            ( battle, sceneModel )
                |> Battle.runPlayerAction action
        
        newMonster =
            newBattle.monster
        
        ( newScene, newSceneModel2, newCmd ) =
            if newBattle.state == Battle.Done then
                ( Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.Escaped)) delve, SceneModel.completeBattle newSceneModel, Cmd.none )
            else if newSceneModel.hitPoints <= 0 then
                ( Scene.GameOverScene, SceneModel.completeBattle newSceneModel, Cmd.none )
            else if newMonster.hitPoints <= 0 then
                ( Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.VictoryLoading newBattle)) delve, newSceneModel, Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster) )
            else
                ( Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.BattleMonster newBattle monsterAction)) delve, newSceneModel, Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle) )

        newSceneModel3 =
            { newSceneModel2
                | actionStates =
                    newSceneModel2.actionStates
                        |> ActionState.performOneAction action
            }
    in
    ( { model | phase = Phase.ScenePhase newScene newSceneModel3 }, newCmd )



updateEndBattleTurn : Model -> Battle -> Action -> SceneModel -> ( Model, Cmd Msg )
updateEndBattleTurn model battle monsterAction sceneModel =
    let
        ( newBattle, newSceneModel ) =
            ( battle, sceneModel )
                |> Battle.runMonsterAction monsterAction
                |> Battle.completeRound
        
        newMonster =
            newBattle.monster
        
        ( newScene, newSceneModel2, newCmd ) =
            if newBattle.state == Battle.Done then
                ( Scene.EscapedScene, SceneModel.completeBattle newSceneModel, Cmd.none )
            else if newSceneModel.hitPoints <= 0 then
                ( Scene.GameOverScene, SceneModel.completeBattle newSceneModel, Cmd.none )
            else if newMonster.hitPoints <= 0 then
                ( Scene.VictoryLoadingScene newBattle, newSceneModel, Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster) )
            else
                ( Scene.BattleMonsterLoadingIntentScene newBattle, Battler.completeRound newSceneModel, Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle) )
        
        newSceneModel3 =
            { newSceneModel2 
                | actionPoints = newSceneModel2.maxActionPoints
                , actionStates =
                    newSceneModel2.actionStates
                        |> List.map ActionState.tick
            }
    in
    ( { model | phase = Phase.ScenePhase newScene newSceneModel3 }, newCmd )

updateDungeonEndBattleTurn : Model -> Battle -> Action -> Delve -> SceneModel -> ( Model, Cmd Msg )
updateDungeonEndBattleTurn model battle monsterAction delve sceneModel =
    let
        ( newBattle, newSceneModel ) =
            ( battle, sceneModel )
                |> Battle.runMonsterAction monsterAction
                |> Battle.completeRound
        
        newMonster =
            newBattle.monster
        
        ( newScene, newSceneModel2, newCmd ) =
            if newBattle.state == Battle.Done then
                ( Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.Escaped)) delve, SceneModel.completeBattle newSceneModel, Cmd.none )
            else if newSceneModel.hitPoints <= 0 then
                ( Scene.GameOverScene, SceneModel.completeBattle newSceneModel, Cmd.none )
            else if newMonster.hitPoints <= 0 then
                ( Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.VictoryLoading newBattle)) delve, newSceneModel, Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster) )
            else
                ( Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.BattleMonsterLoadingIntent newBattle)) delve, Battler.completeRound newSceneModel, Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle) )
        
        newSceneModel3 =
            { newSceneModel2 
                | actionPoints = newSceneModel2.maxActionPoints
                , actionStates =
                    newSceneModel2.actionStates
                        |> List.map ActionState.tick
            }
    in
    ( { model | phase = Phase.ScenePhase newScene newSceneModel3 }, newCmd )

updateBossBattleAction : Model -> Battle -> Action -> Action -> BossState -> SceneModel -> ( Model, Cmd Msg )
updateBossBattleAction model battle action monsterAction state sceneModel =
    let
        ( newBattle, newSceneModel ) =
            ( battle, sceneModel )
                |> Battle.runPlayerAction action
        
        newMonster =
            newBattle.monster
        
        ( newScene, newSceneModel2, newCmd ) =
            if newBattle.state == Battle.Done then
                ( Scene.BossFightScene (BossPhase.ActionPhase (BossScene.Escaped)) state, SceneModel.completeBattle newSceneModel, Cmd.none )
            else if newSceneModel.hitPoints <= 0 then
                ( Scene.GameOverScene, SceneModel.completeBattle newSceneModel, Cmd.none )
            else if newMonster.hitPoints <= 0 then
                ( Scene.BossFightScene (BossPhase.ActionPhase (BossScene.VictoryLoading newBattle)) state, newSceneModel, Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster) )
            else
                ( Scene.BossFightScene (BossPhase.ActionPhase (BossScene.BattleBossOngoing newBattle monsterAction)) state, newSceneModel, Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle) )

        newSceneModel3 =
            { newSceneModel2
                | actionStates =
                    newSceneModel2.actionStates
                        |> ActionState.performOneAction action
            }
    in
    ( { model | phase = Phase.ScenePhase newScene newSceneModel3 }, newCmd )

updateBossEndBattleTurn : Model -> Battle -> Action -> BossState -> SceneModel -> ( Model, Cmd Msg )
updateBossEndBattleTurn model battle monsterAction state sceneModel =
    let
        ( newBattle, newSceneModel ) =
            ( battle, sceneModel )
                |> Battle.runMonsterAction monsterAction
                |> Battle.completeRound
        
        newMonster =
            newBattle.monster
        
        ( newScene, newSceneModel2, newCmd ) =
            if newBattle.state == Battle.Done then
                ( Scene.BossFightScene (BossPhase.ActionPhase (BossScene.Escaped)) state, SceneModel.completeBattle newSceneModel, Cmd.none )
            else if newSceneModel.hitPoints <= 0 then
                ( Scene.GameOverScene, SceneModel.completeBattle newSceneModel, Cmd.none )
            else if newMonster.hitPoints <= 0 then
                ( Scene.BossFightScene (BossPhase.ActionPhase (BossScene.VictoryLoading newBattle)) state, newSceneModel, Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster) )
            else
                ( Scene.BossFightScene (BossPhase.ActionPhase (BossScene.BattleBossLoadingIntent newBattle)) state, Battler.completeRound newSceneModel, Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle) )
        
        newSceneModel3 =
            { newSceneModel2 
                | actionPoints = newSceneModel2.maxActionPoints
                , actionStates =
                    newSceneModel2.actionStates
                        |> List.map ActionState.tick
            }
    in
    ( { model | phase = Phase.ScenePhase newScene newSceneModel3 }, newCmd )