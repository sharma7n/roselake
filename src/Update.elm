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
import SceneState exposing (SceneState)
import Character exposing (Character)

import Phase exposing (Phase)

import Model exposing (Model)
import Msg exposing (Msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.phase ) of        
        ( Msg.UserSelectedScene scene, Phase.ScenePhase _ sceneState character ) ->
            ( { model | phase = Phase.ScenePhase scene sceneState character }, Cmd.none )
        
        ( Msg.UserSelectedMonsterTemplate monsterTemplate, Phase.ScenePhase _ sceneState character ) ->
            let
                battle =
                    Battle.new (Monster.new monsterTemplate)
                
                cmd =
                    Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction battle)
                
                newSceneState =
                    { sceneState | maybeBattle = Just battle }
                
                newCharacter =
                    Character.completeBattle character
            in
            ( { model | phase = Phase.ScenePhase (Scene.BattleMonsterLoadingIntent) newSceneState newCharacter }, cmd )
        
        ( Msg.UserSelectedLearnSkill action, Phase.ScenePhase scene sceneState character ) ->
            let
                newCharacter =
                    if action.learnCost <= character.freeAbilityPoints then
                        { character
                            | freeAbilityPoints =
                                character.freeAbilityPoints - action.learnCost
                            , actions =
                                action :: character.actions
                            , learned =
                                character.learned
                                    |> Set.insert action.id
                        }
                    else
                        character
            in
            ( { model | phase = Phase.ScenePhase scene sceneState newCharacter }, Cmd.none )
        
        ( Msg.UserSelectedLearnPassive passive, Phase.ScenePhase scene sceneState character ) ->
            let
                newCharacter =
                    if passive.learnCost <= character.freeAbilityPoints then
                        { character
                            | freeAbilityPoints =
                                character.freeAbilityPoints - passive.learnCost
                            , passives =
                                passive :: character.passives
                            , learnedPassives =
                                character.learnedPassives
                                    |> Set.insert passive.id
                        }
                    else
                        character
            in
            ( { model | phase = Phase.ScenePhase scene sceneState newCharacter }, Cmd.none )
        
        ( Msg.UserSelectedEquipWeapon weapon, Phase.ScenePhase scene sceneState character ) ->
            let
                newInventory =
                    case character.equippedWeapon of
                        Just heldWeapon ->
                            character.inventory
                                |> Inventory.modifyWeaponQuantity weapon -1
                                |> Inventory.modifyWeaponQuantity heldWeapon 1
                        
                        Nothing ->
                            character.inventory
                                |> Inventory.modifyWeaponQuantity weapon -1
                
                newCharacter =
                    { character
                        | inventory = newInventory
                        , equippedWeapon = Just weapon
                    }
                
                newModel =
                    { model | phase = Phase.ScenePhase scene sceneState newCharacter }
                
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedUnEquipWeapon weapon, Phase.ScenePhase scene sceneState character ) ->
            let
                newCharacter =
                    { character
                        | equippedWeapon = Nothing
                        , inventory =
                            character.inventory
                                |> Inventory.modifyWeaponQuantity weapon 1
                    }
                newModel =
                    { model | phase = Phase.ScenePhase scene sceneState newCharacter }
                
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedEquipArmor armor, Phase.ScenePhase scene sceneState character ) ->
            let
                newInventory =
                    case character.equippedArmor of
                        Just heldArmor ->
                            character.inventory
                                |> Inventory.modifyArmorQuantity armor -1
                                |> Inventory.modifyArmorQuantity heldArmor 1
                        
                        Nothing ->
                            character.inventory
                                |> Inventory.modifyArmorQuantity armor -1
                
                newCharacter =
                    { character
                        | inventory = newInventory
                        , equippedArmor = Just armor
                    }
                
                newModel =
                    { model | phase = Phase.ScenePhase scene sceneState newCharacter }
                
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedUnEquipArmor armor, Phase.ScenePhase scene sceneState character ) ->
            let
                newCharacter =
                    { character
                        | equippedArmor = Nothing
                        , inventory =
                            character.inventory
                                |> Inventory.modifyArmorQuantity armor 1
                    }
                newModel =
                    { model | phase = Phase.ScenePhase scene sceneState newCharacter }
                
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedEquipEssentia idx listIdx essentia, Phase.ScenePhase scene sceneState character ) ->
            let
                equippedEssentia =
                    EssentiaContainer.getSlot idx character.essentiaContainer
                
                newEssentia =
                    character.essentia
                        |> Util.removeListAt listIdx
                        |> Util.appendMaybe equippedEssentia
                
                newEssentiaContainer =
                    character.essentiaContainer
                        |> EssentiaContainer.setSlot idx essentia
                
                newCharacter =
                    { character
                        | essentia = newEssentia
                        , essentiaContainer = newEssentiaContainer
                    }
                
                newModel =
                    { model | phase = Phase.ScenePhase scene sceneState newCharacter }
                
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedUnEquipEssentia idx essentia, Phase.ScenePhase scene sceneState character ) ->
            let
                equippedEssentia =
                    EssentiaContainer.getSlot idx character.essentiaContainer
                
                newEssentia =
                    character.essentia
                        |> Util.appendMaybe equippedEssentia
                
                newEssentiaContainer =
                    character.essentiaContainer
                        |> EssentiaContainer.clearSlot idx
                
                newCharacter =
                    { character
                        | essentia = newEssentia
                        , essentiaContainer = newEssentiaContainer
                    }
                
                newModel =
                    { model | phase = Phase.ScenePhase scene sceneState newCharacter }
                
            in
            ( newModel, Cmd.none )
           
        ( Msg.UserSelectedHomeRest, Phase.ScenePhase scene sceneState character ) ->
            let
                newCharacter =
                    { character 
                        | hitPoints = character.maxHitPoints
                        , magicPoints = character.maxMagicPoints
                    }
            in
            ( { model | phase = Phase.ScenePhase scene sceneState newCharacter }, Cmd.none )
               
        ( Msg.UserSelectedShop shop, Phase.ScenePhase Scene.ShopSelect sceneState character ) ->
            ( { model | phase = Phase.ScenePhase (Scene.Shop shop) sceneState character }, Cmd.none )
        
        ( Msg.UserSelectedBuy item, Phase.ScenePhase scene sceneState character ) ->
            let
                newCharacter =
                    if item.cost <= character.gold then
                        { character
                            | gold = max 0 (character.gold - item.cost)
                            , inventory =
                                character.inventory
                                    |> Inventory.modifyItemQuantity item 1
                        }
                    else
                        character

            in
            ( { model | phase = Phase.ScenePhase scene sceneState newCharacter }, Cmd.none )
        
        ( Msg.UserSelectedUseItem item, Phase.ScenePhase scene sceneState character ) ->
            let
                newCharacter =
                    { character
                        | inventory =
                            character.inventory
                                |> Inventory.modifyItemQuantity item -1
                    }
                        |> Character.applyEffectsToCharacter item.effects
            in
            ( { model | phase = Phase.ScenePhase scene sceneState newCharacter }, Cmd.none )
        
        ( Msg.UserSelectedExploreDungeonScene dungeon, Phase.ScenePhase Scene.Explore _ _ ) ->
            let
                pathListGenerator =
                    Util.randomDistinctList 3 DungeonPath.generator
                
                cmd =
                    Random.generate (Msg.SystemGotDungeonInitialization dungeon) pathListGenerator
            in
            ( model, cmd )
        
        ( Msg.UserSelectedBossFight boss, Phase.ScenePhase _ _ _ ) ->
            let
                pathListGenerator =
                    Util.randomDistinctList 3 ( BossPath.generator boss.bossBehavior )
                
                cmd =
                    Random.generate (Msg.SystemGotBossInitialization boss) pathListGenerator
            in
            ( model, cmd )
        
        ( Msg.SystemGotDungeonInitialization dungeon paths, Phase.ScenePhase Scene.Explore sceneState character ) ->
            let
                delvePhase =
                    DelvePhase.ExplorationPhase paths
                
                delve =
                    { dungeon = dungeon
                    , floor = 1
                    }
                
                newSceneState =
                    { sceneState | ambient = SceneState.Delving delvePhase delve }
            in
            ( { model | phase = Phase.ScenePhase Scene.ExploreDungeon newSceneState character }, Cmd.none )
        
        ( Msg.SystemGotBossInitialization boss paths, Phase.ScenePhase _ sceneState character ) ->
            let
                bossState =
                    { boss = boss
                    , monster = Monster.new boss.monsterTemplate
                    }
                
                newSceneState =
                    { sceneState | ambient = SceneState.BossFight bossState }
                
                newPhase =
                    Phase.ScenePhase ( Scene.BossFight ( BossPhase.ExplorationPhase paths )) newSceneState character
                
                newModel =
                    { model
                        | phase = newPhase
                    }
            in
            ( newModel, Cmd.none )
            
        
        ( Msg.UserSelectedDungeonPath path, Phase.ScenePhase (Scene.ExploreDungeon) _ _) ->
            let
                cmd =
                    Random.generate Msg.SystemGotDungeonScene (Distribution.random path.sceneDistribution)
            in
            ( model, cmd )
        
        ( Msg.UserSelectedBossPath path, Phase.ScenePhase _ _ _ ) ->
            let
                cmd =
                    Random.generate Msg.SystemGotBossScene (Distribution.random path.sceneDistribution)
            in
            ( model, cmd )
        
        ( Msg.SystemGotDungeonScene scene, Phase.ScenePhase Scene.ExploreDungeon sceneState character ) ->
            case sceneState.ambient of
                SceneState.Delving delvePhase delve ->
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
                    ( { model | phase = Phase.ScenePhase Scene.ExploreDungeon sceneState character }, cmd )
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.SystemGotBossScene scene, Phase.ScenePhase (Scene.BossFight _) sceneState character ) ->
            case sceneState.ambient of
                SceneState.BossFight state ->
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

                        newPhase =
                            Phase.ScenePhase (Scene.BossFight (BossPhase.ActionPhase scene)) sceneState character
                        
                        newModel =
                            { model | phase = newPhase }           
                    in
                    ( newModel, cmd )
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.UserSelectedOpenChest, Phase.ScenePhase (Scene.ExploreDungeon) _ _ ) ->
            ( model, Random.generate Msg.SystemGotObject Object.generator )
        
        ( Msg.SystemGotShop shop, Phase.ScenePhase (Scene.ExploreDungeon) sceneState character ) ->
            case sceneState.ambient of
                SceneState.Delving delvePhase delve ->
                    ( { model | phase = Phase.ScenePhase Scene.ExploreDungeon sceneState character }, Cmd.none )
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.SystemGotMonsterTemplate monsterTemplate, Phase.ScenePhase scene sceneState character ) ->
            let
                battle =
                    Battle.new (Monster.new monsterTemplate)
                
                cmd =
                    Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction battle)
                
                newCharacter =
                    Character.completeBattle character
                
                newModel =
                    { model
                        | phase = Phase.ScenePhase scene sceneState newCharacter
                    }
            in
            ( newModel, cmd )
        
        ( Msg.SystemGotMonsterIntent intent, Phase.ScenePhase Scene.ExploreDungeon sceneState character ) ->
            case ( sceneState.ambient, sceneState.maybeBattle ) of
                ( SceneState.Delving delvePhase delve, Just battle ) ->
                    ( { model | phase = Phase.ScenePhase Scene.ExploreDungeon sceneState character }, Cmd.none )
                
                _ ->
                    ( model, Cmd.none )

        ( Msg.SystemGotMonsterIntent intent, Phase.ScenePhase (Scene.BossFight (BossPhase.ActionPhase (BossScene.BattleBossLoadingIntent battle))) sceneState character ) ->
            case sceneState.ambient of
                SceneState.BossFight state ->
                    ( { model | phase = Phase.ScenePhase (Scene.BossFight (BossPhase.ActionPhase (BossScene.BattleBossOngoing battle intent))) sceneState character }, Cmd.none )
                
                _ ->
                    ( model, Cmd.none )

        ( Msg.SystemGotBossMonsterIntent intent, Phase.ScenePhase (Scene.BossFight _) sceneState character ) ->
            case sceneState.ambient of
                SceneState.BossFight state ->
                    let
                        newBattle =
                            Battle.new state.monster
                        
                        newBossPhase =
                            BossPhase.ActionPhase ( BossScene.BattleBossOngoing newBattle intent )
                        
                        newModel =
                            { model | phase = Phase.ScenePhase (Scene.BossFight newBossPhase) sceneState character }
                    in
                    ( newModel, Cmd.none )
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.SystemGotObject object, Phase.ScenePhase (Scene.ExploreDungeon) sceneState character ) ->
            case sceneState.ambient of
                SceneState.Delving delvePhase delve ->
                    let
                        newDungeonScene =
                            DelvePhase.ActionPhase <| DungeonScene.ReceiveTreasure object
                        
                        newInventory =
                            case object of
                                Object.Item i ->
                                    character.inventory
                                        |> Inventory.modifyItemQuantity i 1
                                
                                Object.Weapon w ->
                                    character.inventory
                                        |> Inventory.modifyWeaponQuantity w 1
                                
                                Object.Armor a ->
                                    character.inventory
                                        |> Inventory.modifyArmorQuantity a 1
                        
                        newCharacter =
                            { character
                                | inventory = newInventory
                            }
                        
                        newSceneState =
                             { ambient = SceneState.Delving newDungeonScene delve
                             , maybeBattle = Nothing
                             , maybeMonsterAction = Nothing
                             }
                        
                        newModel =
                            { model
                                | phase = Phase.ScenePhase (Scene.ExploreDungeon) newSceneState newCharacter
                            }
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        
        ( Msg.UserSelectedContinueDungeon, _ ) ->
            let
                pathListGenerator =
                    Util.randomDistinctList 3 DungeonPath.generator

                cmd =
                    Random.generate Msg.SystemGotDungeonContinuation pathListGenerator
            in
            ( model, cmd )
        
        ( Msg.UserSelectedContinueBossFight boss, _ ) ->
            let
                pathListGenerator =
                    Util.randomDistinctList 3 ( BossPath.generator boss.bossBehavior )

                cmd =
                    Random.generate Msg.SystemGotBossFightContinuation pathListGenerator
            in
            ( model, cmd )
        
        ( Msg.SystemGotDungeonContinuation paths, Phase.ScenePhase (Scene.ExploreDungeon) sceneState character ) ->
            case sceneState.ambient of
                SceneState.Delving delvePhase delve ->
                    let
                        cmd =
                            if delve.floor >= delve.dungeon.depth then
                                Random.generate Msg.SystemGotReward (Dungeon.generateReward delve.dungeon)
                            else
                                Cmd.none
                        
                        ( newScene, newDelvePhase, newDelve ) =
                            if delve.floor >= delve.dungeon.depth then
                                ( Scene.ExploreDungeon, (DelvePhase.ActionPhase DungeonScene.LoadingGoal),  delve )
                            else
                                let
                                    newDelve2 =
                                        { delve
                                            | floor =
                                                delve.floor + 1
                                                    |> Util.boundedBy 1 delve.dungeon.depth
                                        }
                                in
                                ( Scene.ExploreDungeon, (DelvePhase.ExplorationPhase paths), newDelve2 )
                        
                        newSceneState =
                            SceneState.Delving newDelvePhase newDelve
                    in 
                    ( { model | phase = Phase.ScenePhase newScene sceneState character }, cmd )
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.SystemGotBossFightContinuation paths, Phase.ScenePhase (Scene.BossFight _) sceneState character ) ->
            case sceneState.ambient of
                SceneState.BossFight state ->
                    let
                        cmd =
                            Cmd.none
                        
                        newScene =
                            Scene.BossFight (BossPhase.ExplorationPhase paths)
                    in 
                    ( { model | phase = Phase.ScenePhase newScene sceneState character }, cmd )
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.SystemGotReward reward, Phase.ScenePhase (Scene.VictoryLoading) sceneState character ) ->
            let
                newCharacter =
                    character
                        |> Character.applyReward reward
                        |> Character.completeBattle
            in
            ( { model | phase = Phase.ScenePhase (Scene.Victory reward) sceneState newCharacter }, Cmd.none )
        
        ( Msg.SystemGotReward reward, Phase.ScenePhase (Scene.ExploreDungeon) sceneState character ) ->
            case ( sceneState.ambient, sceneState.maybeBattle ) of
                ( SceneState.Delving delvePhase delve, Just battle ) ->
                    let
                        newCharacter =
                            character
                                |> Character.applyReward reward
                                |> Character.completeBattle
                    in
                    ( { model | phase = Phase.ScenePhase (Scene.ExploreDungeon) sceneState newCharacter }, Cmd.none )
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.SystemGotMonsterIntent intent, Phase.ScenePhase (Scene.BattleMonsterLoadingIntent) sceneState character ) ->
            ( { model | phase = Phase.ScenePhase (Scene.BattleMonster intent) sceneState character }, Cmd.none )
        
        ( Msg.UserSelectedBattleAction action, Phase.ScenePhase (Scene.BattleMonster monsterAction) sceneState character ) ->
            case sceneState.maybeBattle of
                Just battle ->
                    updateBattleAction model battle action monsterAction character
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.UserSelectedEndBattleTurn, Phase.ScenePhase (Scene.BattleMonster monsterAction) sceneState character ) ->
            case sceneState.maybeBattle of
                Just battle ->
                    updateEndBattleTurn model battle monsterAction character
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.UserSelectedBattleAction action, Phase.ScenePhase Scene.ExploreDungeon sceneState character ) ->
            case ( sceneState.ambient, sceneState.maybeBattle, sceneState.maybeMonsterAction ) of
                ( SceneState.Delving delvePhase delve, Just battle, Just monsterAction ) ->
                    updateDungeonBattleAction model battle action monsterAction delvePhase delve character
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.UserSelectedEndBattleTurn, Phase.ScenePhase (Scene.ExploreDungeon) sceneState character ) ->
            case ( sceneState.ambient, sceneState.maybeBattle, sceneState.maybeMonsterAction ) of
                ( SceneState.Delving delvePhase delve, Just battle, Just monsterAction ) ->
                    updateDungeonEndBattleTurn model battle monsterAction delvePhase delve character
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.UserSelectedBattleAction action, Phase.ScenePhase (Scene.BossFight (BossPhase.ActionPhase (BossScene.BattleBossOngoing battle monsterAction))) sceneState character ) ->
            case sceneState.ambient of
                SceneState.BossFight state ->
                    updateBossBattleAction model battle action monsterAction state character
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.UserSelectedEndBattleTurn, Phase.ScenePhase (Scene.BossFight (BossPhase.ActionPhase (BossScene.BattleBossOngoing battle monsterAction))) sceneState character ) ->
            case sceneState.ambient of
                SceneState.BossFight state ->
                    updateBossEndBattleTurn model battle monsterAction state character
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.UserSelectedRest, Phase.ScenePhase (Scene.ExploreDungeon) sceneState character ) ->
            case sceneState.ambient of
                SceneState.Delving delvePhase delve ->
                    let
                        newCharacter =
                            { character
                                | hitPoints = character.maxHitPoints
                                , magicPoints = character.maxMagicPoints
                            }
                        
                        newModel =
                            { model
                                | phase = Phase.ScenePhase (Scene.ExploreDungeon) sceneState newCharacter
                            }
                    in
                    ( newModel, Cmd.none )
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.UserSelectedExitDungeon, Phase.ScenePhase Scene.ExploreDungeon sceneState character ) ->
            case sceneState.ambient of
                SceneState.Delving delvePhase delve ->
                    ( { model | phase = Phase.ScenePhase Scene.Explore sceneState character }, Cmd.none )
                
                _ ->
                    ( model, Cmd.none )
        
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
                    else if d < 0 && c.attributePoints < 20 && CharacterCreationModel.getAttribute attr c > 1 then
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
        
        ( Msg.UserSelectedOnyxTower, Phase.ScenePhase _ sceneState character ) ->
            ( { model | phase = Phase.ScenePhase Scene.OnyxTower sceneState character }, Cmd.none )
        
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
        
        characterResult =
            Character.characterCreationModelToCharacter { characterCreationModel | settings = newSettings }
        
        newModel =
            case characterResult of
                Ok character ->
                    { model 
                        | phase = Phase.ScenePhase Scene.Player SceneState.new character 
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
        
        characterResult =
            Character.characterCreationModelToCharacter { characterCreationModel | settings = newSettings }
        
        newModel =
            case characterResult of
                Ok character ->
                    let
                        devCharacter =
                            { character
                                | freeAbilityPoints = 100
                                , totalAbilityPoints = 100
                            }
                    in
                    { model | phase = Phase.ScenePhase Scene.Player SceneState.new devCharacter }
                
                Err _ ->
                    { model | phase = Phase.CharacterCreationPhase { characterCreationModel | settings = newSettings }}

    in
    ( newModel, Cmd.none )

updateBattleAction : Model -> Battle -> Action -> Action -> Character -> ( Model, Cmd Msg )
updateBattleAction model battle action monsterAction character =
    let
        ( newBattle, newCharacter ) =
            ( battle, character )
                |> Battle.runPlayerAction action
        
        newMonster =
            newBattle.monster
        
        next =
            if newBattle.state == Battle.Done then
                { scene = Scene.Escaped
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                , sceneState = SceneState.new 
                }
            else if newCharacter.hitPoints <= 0 then
                { scene = Scene.GameOver
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                , sceneState = SceneState.new
                }
            else if newMonster.hitPoints <= 0 then
                { scene = Scene.VictoryLoading
                , character = newCharacter
                , cmd = Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster)
                , sceneState = 
                    { ambient = SceneState.Rest
                    , maybeBattle = Just newBattle
                    , maybeMonsterAction = Nothing 
                    }
                }
            else
                { scene = Scene.BattleMonster monsterAction
                , character = newCharacter
                , cmd = Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle)
                , sceneState = 
                    { ambient = SceneState.Rest
                    , maybeBattle = Just newBattle
                    , maybeMonsterAction = Nothing
                    }
                }
        
        newCharacter2 = next.character

        newCharacter3 =
            { newCharacter2
                | actionStates =
                    newCharacter2.actionStates
                        |> ActionState.performOneAction action
            }
    in
    ( { model | phase = Phase.ScenePhase next.scene next.sceneState newCharacter3 }, next.cmd )

updateDungeonBattleAction : Model -> Battle -> Action -> Action -> DelvePhase -> Delve -> Character -> ( Model, Cmd Msg )
updateDungeonBattleAction model battle action monsterAction delvePhase delve character =
    let
        ( newBattle, newCharacter ) =
            ( battle, character )
                |> Battle.runPlayerAction action
        
        newMonster =
            newBattle.monster
        
        next =
            if newBattle.state == Battle.Done then
                { scene = Scene.ExploreDungeon
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                , sceneState =
                    { ambient = SceneState.Delving delvePhase delve
                    , maybeBattle = Nothing
                    , maybeMonsterAction = Nothing
                    }
                }
            else if newCharacter.hitPoints <= 0 then
                { scene = Scene.GameOver
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                , sceneState = SceneState.new
                }
            else if newMonster.hitPoints <= 0 then
                { scene = Scene.ExploreDungeon
                , character = newCharacter
                , cmd = Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster)
                , sceneState =
                    { ambient = SceneState.Delving delvePhase delve
                    , maybeBattle = Nothing
                    , maybeMonsterAction = Nothing
                    }
                }
            else
                { scene = Scene.ExploreDungeon
                , character = newCharacter
                , cmd = Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle)
                , sceneState = 
                    { ambient = SceneState.Delving delvePhase delve
                    , maybeBattle = Just newBattle
                    , maybeMonsterAction = Nothing
                    }
                }

        newCharacter2 = next.character

        newCharacter3 =
            { newCharacter2
                | actionStates =
                    newCharacter2.actionStates
                        |> ActionState.performOneAction action
            }
    in
    ( { model | phase = Phase.ScenePhase next.scene next.sceneState newCharacter3 }, next.cmd )



updateEndBattleTurn : Model -> Battle -> Action -> Character -> ( Model, Cmd Msg )
updateEndBattleTurn model battle monsterAction character =
    let
        ( newBattle, newCharacter ) =
            ( battle, character )
                |> Battle.runMonsterAction monsterAction
                |> Battle.completeRound
        
        newMonster =
            newBattle.monster
        
        next =
            if newBattle.state == Battle.Done then
                { scene = Scene.Escaped
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                , sceneState = SceneState.new
                }
            else if newCharacter.hitPoints <= 0 then
                { scene = Scene.GameOver
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                , sceneState = SceneState.new
                }
            else if newMonster.hitPoints <= 0 then
                { scene = Scene.VictoryLoading
                , character = newCharacter
                , cmd = Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster)
                , sceneState = SceneState.new
                }
            else
                { scene = Scene.BattleMonsterLoadingIntent
                , character = Battler.completeRound newCharacter
                , cmd = Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle)
                , sceneState =
                    { ambient = SceneState.Rest
                    , maybeBattle = Just newBattle
                    , maybeMonsterAction = Nothing
                    }
                }
        
        newCharacter2 = next.character
        newCharacter3 =
            { newCharacter2 
                | actionPoints = newCharacter2.maxActionPoints
                , actionStates =
                    newCharacter2.actionStates
                        |> List.map ActionState.tick
            }
    in
    ( { model | phase = Phase.ScenePhase next.scene next.sceneState newCharacter3 }, next.cmd )

updateDungeonEndBattleTurn : Model -> Battle -> Action -> DelvePhase -> Delve -> Character -> ( Model, Cmd Msg )
updateDungeonEndBattleTurn model battle monsterAction delvePhase delve character =
    let
        ( newBattle, newCharacter ) =
            ( battle, character )
                |> Battle.runMonsterAction monsterAction
                |> Battle.completeRound
        
        newMonster =
            newBattle.monster
        
        next =
            if newBattle.state == Battle.Done then
                { scene = Scene.ExploreDungeon
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                , sceneState =
                    { ambient = SceneState.Delving delvePhase delve
                    , maybeBattle = Nothing
                    , maybeMonsterAction = Nothing
                    }
                }
            else if newCharacter.hitPoints <= 0 then
                { scene = Scene.GameOver
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                , sceneState = SceneState.new
                }
            else if newMonster.hitPoints <= 0 then
                { scene = Scene.ExploreDungeon
                , character = newCharacter
                , cmd = Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster)
                , sceneState =
                    { ambient = SceneState.Delving delvePhase delve
                    , maybeBattle = Nothing
                    , maybeMonsterAction = Nothing
                    }
                }
            else
                { scene = Scene.ExploreDungeon
                , character = Battler.completeRound newCharacter
                , cmd = Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle)
                , sceneState =
                    { ambient = SceneState.Delving delvePhase delve
                    , maybeBattle = Just battle
                    , maybeMonsterAction = Nothing
                    }
                }
        
        newCharacter2 = next.character

        newCharacter3 =
            { newCharacter2 
                | actionPoints = newCharacter2.maxActionPoints
                , actionStates =
                    newCharacter2.actionStates
                        |> List.map ActionState.tick
            }
    in
    ( { model | phase = Phase.ScenePhase next.scene next.sceneState newCharacter3 }, next.cmd )

updateBossBattleAction : Model -> Battle -> Action -> Action -> BossState -> Character -> ( Model, Cmd Msg )
updateBossBattleAction model battle action monsterAction state character =
    let
        ( newBattle, newCharacter ) =
            ( battle, character )
                |> Battle.runPlayerAction action
        
        newMonster =
            newBattle.monster
        
        newState =
            { state | monster = newMonster }
        
        next =
            if newBattle.state == Battle.Done then
                { scene = Scene.BossFight (BossPhase.ActionPhase (BossScene.Escaped))
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                , sceneState =
                    { ambient = SceneState.BossFight newState
                    , maybeBattle = Nothing
                    , maybeMonsterAction = Nothing
                    }
                }
            else if newCharacter.hitPoints <= 0 then
                { scene = Scene.GameOver
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                , sceneState =
                    { ambient = SceneState.BossFight newState
                    , maybeBattle = Nothing
                    , maybeMonsterAction = Nothing
                    }
                }
            else if newMonster.hitPoints <= 0 then
                { scene = Scene.BossFight (BossPhase.ActionPhase (BossScene.VictoryLoading newBattle))
                , character = newCharacter
                , cmd = Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster)
                , sceneState =
                    { ambient = SceneState.BossFight newState
                    , maybeBattle = Nothing
                    , maybeMonsterAction = Nothing
                    }
                }
            else
                { scene = Scene.BossFight (BossPhase.ActionPhase (BossScene.BattleBossOngoing newBattle monsterAction))
                , character = newCharacter
                , cmd = Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle)
                , sceneState =
                    { ambient = SceneState.BossFight newState
                    , maybeBattle = Just battle
                    , maybeMonsterAction = Nothing
                    }
                }

        newCharacter2 = next.character

        newCharacter3 =
            { newCharacter2
                | actionStates =
                    newCharacter2.actionStates
                        |> ActionState.performOneAction action
            }
    in
    ( { model | phase = Phase.ScenePhase next.scene next.sceneState newCharacter3 }, next.cmd )

updateBossEndBattleTurn : Model -> Battle -> Action -> BossState -> Character -> ( Model, Cmd Msg )
updateBossEndBattleTurn model battle monsterAction state character =
    let
        ( newBattle, newCharacter ) =
            ( battle, character )
                |> Battle.runMonsterAction monsterAction
                |> Battle.completeRound
        
        newMonster =
            newBattle.monster
        
        newState =
            { state | monster = newMonster }
        
        next =
            if newBattle.state == Battle.Done then
                { scene = Scene.BossFight (BossPhase.ActionPhase (BossScene.Escaped))
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                , sceneState =
                    { ambient = SceneState.BossFight newState
                    , maybeBattle = Nothing
                    , maybeMonsterAction = Nothing
                    }
                }
            else if newCharacter.hitPoints <= 0 then
                { scene = Scene.GameOver
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                , sceneState =
                    { ambient = SceneState.BossFight newState
                    , maybeBattle = Nothing
                    , maybeMonsterAction = Nothing
                    }
                }
            else if newMonster.hitPoints <= 0 then
                { scene = Scene.BossFight (BossPhase.ActionPhase (BossScene.VictoryLoading newBattle))
                , character = newCharacter
                , cmd = Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster)
                , sceneState =
                    { ambient = SceneState.BossFight newState
                    , maybeBattle = Nothing
                    , maybeMonsterAction = Nothing
                    }
                }
            else
                { scene = Scene.BossFight (BossPhase.ActionPhase (BossScene.BattleBossLoadingIntent newBattle))
                , character = Battler.completeRound newCharacter
                , cmd = Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle)
                , sceneState =
                    { ambient = SceneState.BossFight newState
                    , maybeBattle = Just battle
                    , maybeMonsterAction = Nothing
                    }
                }
        
        newCharacter2 = next.character

        newCharacter3 =
            { newCharacter2 
                | actionPoints = newCharacter2.maxActionPoints
                , actionStates =
                    newCharacter2.actionStates
                        |> List.map ActionState.tick
            }
    in
    ( { model | phase = Phase.ScenePhase next.scene next.sceneState newCharacter3 }, next.cmd )

updateGenericBattleAction : Action -> Action -> Character -> Battle -> SceneState -> Model -> ( Model, Cmd Msg )
updateGenericBattleAction characterAction monsterAction character battle sceneState m =
    let
        ( newBattle, newCharacter ) =
            ( battle, character )
                |> Battle.runPlayerAction characterAction
        
        newMonster =
            newBattle.monster
        
        newSceneState =
            case sceneState.ambient of
                SceneState.Rest ->
                    sceneState
                        |> SceneState.setBattle newBattle
                
                SceneState.Delving delvePhase delve ->
                    sceneState
                        |> SceneState.setBattle newBattle

                SceneState.BossFight bossState ->
                    let
                        newBossState =
                            { bossState | monster = newMonster }
                    in
                    { ambient = SceneState.Rest
                    , maybeBattle = Just newBattle
                    , maybeMonsterAction = Nothing
                    }
        
        next =
            if newBattle.state == Battle.Done then
                { scene = Scene.Escaped
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                }
            else if newCharacter.hitPoints <= 0 then
                { scene = Scene.GameOver
                , character = Character.completeBattle newCharacter
                , cmd = Cmd.none
                }
            else if newMonster.hitPoints <= 0 then
                { scene = Scene.VictoryLoading
                , character = Character.completeBattle newCharacter
                , cmd = Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster)
                }
            else
                { scene = Scene.BattleMonster monsterAction
                , character = newCharacter
                , cmd = Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle)
                }

        newCharacter2 = next.character

        newCharacter3 =
            { newCharacter2
                | actionStates =
                    newCharacter2.actionStates
                        |> ActionState.performOneAction monsterAction
            }
    in
    ( { m | phase = Phase.ScenePhase next.scene newSceneState newCharacter3 }, next.cmd )