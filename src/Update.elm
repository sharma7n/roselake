module Update exposing
    ( update
    )

import Random
import Set exposing (Set)

import Maybe.Extra

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
               
        ( Msg.UserSelectedShop shop, Phase.ScenePhase scene sceneState character ) ->
            let
                newSceneState =
                    sceneState
                        |> SceneState.setShop shop
            in
            ( { model | phase = Phase.ScenePhase scene newSceneState character }, Cmd.none )
        
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
        
        ( Msg.UserSelectedExploreDungeonScene dungeon, Phase.ScenePhase Scene.DungeonSelect _ _ ) ->
            let
                pathListGenerator =
                    Util.randomDistinctList 3 DungeonPath.generator
                
                cmd =
                    Random.generate (Msg.SystemGotDungeonInitialization dungeon) pathListGenerator
            in
            ( model, cmd )
        
        ( Msg.SystemGotDungeonInitialization dungeon paths, Phase.ScenePhase _ sceneState character ) ->
            let
                delvePhase =
                    DelvePhase.ExplorationPhase paths
                
                delve =
                    { dungeon = dungeon
                    , floor = 1
                    }
                
                newSceneState =
                    { sceneState 
                        | ambient = SceneState.Delving delvePhase delve 
                    }
            in
            ( { model | phase = Phase.ScenePhase Scene.ExploreDungeon newSceneState character }, Cmd.none )  
        
        ( Msg.UserSelectedDungeonPath path, Phase.ScenePhase Scene.ExploreDungeon _ _) ->
            let
                cmd =
                    Random.generate Msg.SystemGotDungeonScene (Distribution.random path.sceneDistribution)
            in
            ( model, cmd )
        
        ( Msg.SystemGotDungeonScene scene, Phase.ScenePhase Scene.ExploreDungeon sceneState character ) ->
            case sceneState.ambient of
                SceneState.Delving delvePhase delve ->
                    let
                        newDelvePhase =
                            DelvePhase.ActionPhase scene
                        
                        newSceneState =
                            { sceneState
                                | ambient = SceneState.Delving newDelvePhase delve
                            }
                        
                        cmd =
                            case scene of
                                DungeonScene.Battle ->
                                    Random.generate Msg.SystemGotMonsterTemplate (Dungeon.generateMonsterTemplate delve.dungeon)
                                
                                DungeonScene.Shop ->
                                    Random.generate Msg.SystemGotShop Shop.generator
                                
                                _ ->
                                    Cmd.none
                        
                    in
                    ( { model | phase = Phase.ScenePhase Scene.ExploreDungeon newSceneState character }, cmd )
                
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
                
                newSceneState =
                    sceneState
                        |> SceneState.setBattle battle
                
                cmd =
                    Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction battle)
                
                newCharacter =
                    Character.completeBattle character
                
                newModel =
                    { model
                        | phase = Phase.ScenePhase scene newSceneState newCharacter
                    }
            in
            ( newModel, cmd )
        
        ( Msg.SystemGotMonsterIntent intent, Phase.ScenePhase _ sceneState character ) ->
            let
                newSceneState =
                    sceneState
                        |> SceneState.setMonsterAction intent
                
                newPhase =
                    Phase.ScenePhase Scene.BattleMonster newSceneState character
            in
            ( { model | phase = newPhase }, Cmd.none )
        
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
                            SceneState.new (SceneState.Delving newDungeonScene delve)
                        
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
        
        ( Msg.SystemGotDungeonContinuation paths, Phase.ScenePhase _ sceneState character ) ->
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
                            SceneState.new (SceneState.Delving newDelvePhase newDelve)
                    in 
                    ( { model | phase = Phase.ScenePhase newScene newSceneState character }, cmd )
                
                _ ->
                    ( model, Cmd.none )
        
        ( Msg.SystemGotReward reward, Phase.ScenePhase _ sceneState character ) ->
            let
                newCharacter =
                    character
                        |> Character.applyReward reward
                        |> Character.completeBattle
                
                newSceneState =
                    sceneState
                        |> SceneState.setReward reward
            in
            ( { model | phase = Phase.ScenePhase Scene.Victory newSceneState newCharacter }, Cmd.none )
        
        ( Msg.UserSelectedBattleAction action, Phase.ScenePhase scene sceneState character ) ->
            updateBattleAction model action scene sceneState character
        
        ( Msg.UserSelectedEndBattleTurn, Phase.ScenePhase scene sceneState character ) ->
            updateBattleEndTurn model scene sceneState character
        
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
                    ( { model | phase = Phase.ScenePhase Scene.DungeonSelect sceneState character }, Cmd.none )
                
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
                        | phase = Phase.ScenePhase Scene.Player (SceneState.new SceneState.Rest) character 
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
                    { model | phase = Phase.ScenePhase Scene.Player (SceneState.new SceneState.Rest) devCharacter }
                
                Err _ ->
                    { model | phase = Phase.CharacterCreationPhase { characterCreationModel | settings = newSettings }}

    in
    ( newModel, Cmd.none )

updateBattleAction : Model -> Action -> Scene -> SceneState -> Character -> ( Model, Cmd Msg )
updateBattleAction model action scene sceneState character =
    let
        f battle monsterAction =
            let
                ( newBattle, newCharacter ) =
                    ( battle, character )
                        |> Battle.runPlayerAction action
                
                newMonster =
                    newBattle.monster
                
                newSceneState =
                    sceneState
                        |> SceneState.setBattle newBattle
                
                next =
                    if newBattle.state == Battle.Done then
                        { scene = Scene.Escaped
                        , character = Character.completeBattle newCharacter
                        , cmd = Cmd.none
                        , sceneState =
                            newSceneState
                                |> SceneState.clearBattle
                        }
                    else if newCharacter.hitPoints <= 0 then
                        { scene = Scene.GameOver
                        , character = Character.completeBattle newCharacter
                        , cmd = Cmd.none
                        , sceneState =
                            newSceneState
                                |> SceneState.clearBattle
                        }
                    else if newMonster.hitPoints <= 0 then
                        { scene = Scene.VictoryLoading
                        , character = Character.completeBattle newCharacter
                        , cmd = Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster)
                        , sceneState =
                            newSceneState
                        }
                    else
                        { scene = Scene.BattleMonster
                        , character = newCharacter
                        , cmd = Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle)
                        , sceneState =
                            newSceneState
                        }

                newCharacter2 = next.character

                newCharacter3 =
                    { newCharacter2
                        | actionStates =
                            newCharacter2.actionStates
                                |> ActionState.performOneAction monsterAction
                    }
            in
            ( { model | phase = Phase.ScenePhase next.scene next.sceneState newCharacter3 }, next.cmd )
    in
    Just f
        |> Maybe.Extra.andMap sceneState.maybeBattle
        |> Maybe.Extra.andMap sceneState.maybeMonsterAction
        |> Maybe.withDefault ( model, Cmd.none )
 
updateBattleEndTurn : Model -> Scene -> SceneState -> Character -> ( Model, Cmd Msg )
updateBattleEndTurn model scene sceneState character =
    let
        f battle monsterAction =
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
                        , sceneState = 
                            sceneState
                                |> SceneState.clearBattle
                        }
                    else if newCharacter.hitPoints <= 0 then
                        { scene = Scene.GameOver
                        , character = Character.completeBattle newCharacter
                        , cmd = Cmd.none
                        , sceneState = 
                            sceneState
                                |> SceneState.clearBattle
                        }
                    else if newMonster.hitPoints <= 0 then
                        { scene = Scene.VictoryLoading
                        , character = newCharacter
                        , cmd = Random.generate Msg.SystemGotReward (Monster.generateReward newBattle.monster)
                        , sceneState =
                            sceneState
                        }
                    else
                        { scene = Scene.BattleMonster
                        , character = Battler.completeRound newCharacter
                        , cmd = Random.generate Msg.SystemGotMonsterIntent (Battle.chooseMonsterAction newBattle)
                        , sceneState =
                            sceneState
                                |> SceneState.setBattle newBattle
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
    in
    Just f
        |> Maybe.Extra.andMap sceneState.maybeBattle
        |> Maybe.Extra.andMap sceneState.maybeMonsterAction
        |> Maybe.withDefault ( model, Cmd.none )