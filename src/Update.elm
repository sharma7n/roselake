module Update exposing
    ( update
    )

import Random

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

import Battler exposing (Battler)
import Target exposing (Target)
import Avatar exposing (Avatar)
import Delve exposing (Delve)
import DelvePhase exposing (DelvePhase)
import DungeonPath
import DungeonScene
import Dungeon exposing (Dungeon)
import Action exposing (Action)
import Effect exposing (Effect)
import Monster exposing (Monster)
import Reward exposing (Reward)
import Inventory exposing (Inventory)
import Object exposing (Object)
import Shop exposing (Shop)
import Item exposing (Item)
import Weapon exposing (Weapon)

import Scene exposing (Scene)
import SceneModel exposing (SceneModel)

import Phase exposing (Phase)

import Model exposing (Model)
import Msg exposing (Msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.phase ) of
        ( Msg.UserSelectedPlayerScene, Phase.ScenePhase _ sceneModel ) ->
            ( { model | phase = Phase.ScenePhase Scene.PlayerScene sceneModel }, Cmd.none )
        
        ( Msg.UserSelectedLearnSelectScene, Phase.ScenePhase _ sceneModel ) ->
            ( { model | phase = Phase.ScenePhase Scene.LearnSelectScene sceneModel }, Cmd.none )
        
        ( Msg.UserSelectedLearnSkill action, Phase.ScenePhase scene sceneModel ) ->
            let
                newSceneModel =
                    if action.learnCost <= sceneModel.freeAbilityPoints then
                        { sceneModel
                            | freeAbilityPoints =
                                sceneModel.freeAbilityPoints - action.learnCost
                            , actions =
                                action :: sceneModel.actions
                        }
                    else
                        sceneModel
            in
            ( { model | phase = Phase.ScenePhase scene newSceneModel }, Cmd.none )
        
        ( Msg.UserSelectedHomeScene, Phase.ScenePhase _ sceneModel ) ->
            ( { model | phase = Phase.ScenePhase Scene.HomeScene sceneModel }, Cmd.none )
        
        ( Msg.UserSelectedHomeRest, Phase.ScenePhase scene sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel 
                        | hitPoints = sceneModel.maxHitPoints
                        , magicPoints = sceneModel.maxMagicPoints
                    }
            in
            ( { model | phase = Phase.ScenePhase scene newSceneModel }, Cmd.none )
        
        ( Msg.UserSelectedShopSelectScene, Phase.ScenePhase _ sceneModel ) ->
            ( { model | phase = Phase.ScenePhase Scene.ShopSelectScene sceneModel }, Cmd.none )
        
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
                                    |> Inventory.modify item 1
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
                                |> Inventory.modify item -1
                    }
                        |> SceneModel.applyEffectsToSceneModel item.effects
            in
            ( { model | phase = Phase.ScenePhase scene newSceneModel }, Cmd.none )
        
        ( Msg.UserSelectedExploreScene, Phase.ScenePhase _ sceneModel ) ->
            ( { model | phase = Phase.ScenePhase Scene.ExploreScene sceneModel }, Cmd.none )
        
        ( Msg.UserSelectedExploreDungeonScene dungeon, Phase.ScenePhase Scene.ExploreScene _ ) ->
            let
                pathListGenerator =
                    Random.list 3 DungeonPath.generator
                
                cmd =
                    Random.generate (Msg.SystemGotDungeonInitialization dungeon) pathListGenerator
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
        
        ( Msg.UserSelectedDungeonPath path, Phase.ScenePhase (Scene.ExploreDungeonScene _ _) _) ->
            let
                cmd =
                    Random.generate Msg.SystemGotDungeonScene (Distribution.random path.sceneDistribution)
            in
            ( model, cmd )
        
        ( Msg.SystemGotDungeonScene scene, Phase.ScenePhase (Scene.ExploreDungeonScene delvePhase delve) sceneModel ) ->
            let
                cmd =
                    case scene of
                        DungeonScene.Battle ->
                            Random.generate Msg.SystemGotMonster Monster.generator
                        
                        DungeonScene.Shop ->
                            Random.generate Msg.SystemGotShop Shop.generator
                        
                        _ ->
                            Cmd.none
                
            in
            ( { model | phase = Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase scene) delve) sceneModel }, cmd )
        
        ( Msg.UserSelectedOpenChest, Phase.ScenePhase (Scene.ExploreDungeonScene _ _) _ ) ->
            ( model, Random.generate Msg.SystemGotObject Object.generator )
        
        ( Msg.SystemGotShop shop, Phase.ScenePhase (Scene.ExploreDungeonScene _ delve) sceneModel ) ->
            ( { model | phase = Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.Shopping shop)) delve) sceneModel }, Cmd.none )
        
        ( Msg.SystemGotMonster monster, Phase.ScenePhase (Scene.ExploreDungeonScene delvePhase delve) sceneModel ) ->
            let
                cmd =
                    Random.generate Msg.SystemGotMonsterIntent (Monster.chooseAction monster)
            in
            ( { model | phase = Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.BattleMonsterLoadingIntent monster)) delve) sceneModel }, cmd )
        
        ( Msg.SystemGotMonsterIntent intent, Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.BattleMonsterLoadingIntent monster)) delve) sceneModel ) ->
            ( { model | phase = Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.BattleMonster monster intent)) delve) sceneModel }, Cmd.none )

        ( Msg.SystemGotObject (Object.Item i), Phase.ScenePhase (Scene.ExploreDungeonScene _ delve) sceneModel ) ->
            let
                newDungeonScene =
                    DelvePhase.ActionPhase <| DungeonScene.ReceiveTreasure (Object.Item i)
                
                newSceneModel =
                    { sceneModel
                        | inventory =
                            sceneModel.inventory
                                |> Inventory.modify i 1
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
                        Random.generate Msg.SystemGotReward (generateDungeonReward delve.dungeon)
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
        
        ( Msg.SystemGotReward reward, Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase DungeonScene.LoadingGoal) delve) sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel
                        | experience = sceneModel.experience + reward.experience
                        , gold = sceneModel.gold + reward.gold
                        , freeAbilityPoints = sceneModel.freeAbilityPoints + reward.abilityPoints
                        , totalAbilityPoints = sceneModel.totalAbilityPoints + reward.abilityPoints
                    }
                
                newSceneModel2 =
                        List.foldl (\(item, qty) -> \s ->
                            { s | inventory = Inventory.modify item qty s.inventory }
                        ) newSceneModel reward.items
                
                newModel =
                    { model
                        | phase = Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.Goal reward)) delve) newSceneModel2
                    }
            in
            ( newModel, Cmd.none )
        
        ( Msg.UserSelectedBattleScene, Phase.ScenePhase _ sceneModel ) ->
            ( { model | phase = Phase.ScenePhase Scene.BattleScene sceneModel }, Cmd.none )
        
        ( Msg.UserSelectedBattleMonsterScene monster, Phase.ScenePhase _ sceneModel ) ->
            let
                cmd =
                    Random.generate Msg.SystemGotMonsterIntent (Monster.chooseAction monster)
            in
            ( { model | phase = Phase.ScenePhase (Scene.BattleMonsterLoadingIntentScene monster) sceneModel }, cmd )
        
        ( Msg.SystemGotMonsterIntent intent, Phase.ScenePhase (Scene.BattleMonsterLoadingIntentScene monster) sceneModel ) ->
            ( { model | phase = Phase.ScenePhase (Scene.BattleMonsterScene monster intent) sceneModel }, Cmd.none )
        
        ( Msg.UserSelectedBattleAction action, Phase.ScenePhase (Scene.BattleMonsterScene monster monsterAction) sceneModel ) ->
            updateBattleAction model monster action monsterAction sceneModel
        
        ( Msg.UserSelectedBattleAction action, Phase.ScenePhase (Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.BattleMonster monster monsterAction)) delve) sceneModel ) ->
            updateDungeonBattleAction model monster action monsterAction delve sceneModel
        
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
        
        ( Msg.UserSelectedCharacterCreationConfirmation, Phase.CharacterCreationPhase characterCreationModel ) ->
            updateCharacterCreationConfirmation model characterCreationModel
        
        ( Msg.DevSelectedCharacterCreationConfirmation, Phase.CharacterCreationPhase _ ) ->
            let
                avatar =
                    { hairStyle = HairStyle.Plain
                    , hairColor = HairColor.Brown
                    , complexion = Complexion.Medium
                    , eyeColor = EyeColor.Brown
                    , height = Height.Average
                    , build = Build.Sturdy
                    }
                
                sceneModel =
                    { name = "Dev"
                    , avatar = avatar
                    , gold = 10
                    , inventory = Inventory.new
                    , level = 1
                    , experience = 0
                    , freeAbilityPoints = 0
                    , totalAbilityPoints = 0
                    , satiety = 10
                    , maxSatiety = 10
                    , hitPoints = 10
                    , maxHitPoints = 10
                    , magicPoints = 5
                    , maxMagicPoints = 5
                    , attack = 1
                    , agility = 1
                    , actions =
                        [ Action.byId "attack"
                        , Action.byId "fireball"
                        ]
                    , equippedWeapon = Just <| Weapon.byId "sword"
                    }
                
                newModel =
                    { model | phase = Phase.ScenePhase Scene.PlayerScene sceneModel } 
            in
            ( newModel, Cmd.none )
        
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
    
        newCharacterCreationModel =
            { settings = newSettings
            }
        
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
            SceneModel.characterCreationSettingsToSceneModel newSettings
        
        newModel =
            case sceneModelResult of
                Ok sceneModel ->
                    { model | phase = Phase.ScenePhase Scene.PlayerScene sceneModel }
                
                Err _ ->
                    { model | phase = Phase.CharacterCreationPhase { settings = newSettings }}

    in
    ( newModel, Cmd.none )

updateBattleAction : Model -> Monster -> Action -> Action -> SceneModel -> ( Model, Cmd Msg )
updateBattleAction model monster action monsterAction sceneModel =
    let
        ( newMonster, newSceneModel ) =
            runOneBattleRound monsterAction action monster sceneModel
        
        ( newScene, newSceneModel2 ) =
            if newSceneModel.hitPoints <= 0 then
                ( Scene.GameOverScene, newSceneModel )
            else if newMonster.hitPoints <= 0 then
                let
                    reward =
                        { experience = newMonster.experience
                        , gold = newMonster.gold
                        , abilityPoints = newMonster.abilityPoints
                        , items = []
                        }
                    
                    newSceneModel3 =
                        { newSceneModel
                            | experience = max 0 (newSceneModel.experience + reward.experience)
                            , gold = max 0 (newSceneModel.gold + reward.gold)
                            , freeAbilityPoints = sceneModel.freeAbilityPoints + reward.abilityPoints
                            , totalAbilityPoints = sceneModel.totalAbilityPoints + reward.abilityPoints
                        }
                in
                ( Scene.VictoryScene newMonster reward, newSceneModel3 )

            else
                ( Scene.BattleMonsterLoadingIntentScene newMonster, newSceneModel )
    in
    ( { model | phase = Phase.ScenePhase newScene newSceneModel2 }, Random.generate Msg.SystemGotMonsterIntent (Monster.chooseAction monster) )

updateDungeonBattleAction : Model -> Monster -> Action -> Action -> Delve -> SceneModel -> ( Model, Cmd Msg )
updateDungeonBattleAction model monster action monsterAction delve sceneModel =
    let
        ( newMonster, newSceneModel ) =
            runOneBattleRound monsterAction action monster sceneModel
        
        ( newScene, newSceneModel2 ) =
            if newSceneModel.hitPoints <= 0 then
                ( Scene.GameOverScene, newSceneModel )
            else if newMonster.hitPoints <= 0 then
                let
                    reward =
                        { experience = newMonster.experience
                        , gold = newMonster.gold
                        , abilityPoints = newMonster.abilityPoints
                        , items = []
                        }
                    
                    newSceneModel3 =
                        { newSceneModel
                            | experience = max 0 (newSceneModel.experience + reward.experience)
                            , gold = max 0 (newSceneModel.gold + reward.gold)
                            , freeAbilityPoints = sceneModel.freeAbilityPoints + reward.abilityPoints
                            , totalAbilityPoints = sceneModel.totalAbilityPoints + reward.abilityPoints
                        }
                in
                ( Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.Victory newMonster reward)) delve, newSceneModel3 )

            else
                ( Scene.ExploreDungeonScene (DelvePhase.ActionPhase (DungeonScene.BattleMonsterLoadingIntent newMonster)) delve, newSceneModel )
    in
    ( { model | phase = Phase.ScenePhase newScene newSceneModel2 }, Random.generate Msg.SystemGotMonsterIntent (Monster.chooseAction monster) )


runOneBattleRound : Action -> Action -> Battler a -> Battler b -> ( Battler a, Battler b )
runOneBattleRound actionA actionB battlerA battlerB =
    let
        realActionA =
            if actionA.magicPointCost <= battlerA.magicPoints then
                actionA
            else
                Action.byId "null"
        
        realActionB =
            if actionB.magicPointCost <= battlerB.magicPoints then
                actionB
            else
                Action.byId "null"
        
        battlerAEffects =
            realActionA.subs
                |> List.map (\s -> s.effects)
                |> List.concat
        
        battlerBEffects =
            realActionB.subs
                |> List.map (\s -> s.effects)
                |> List.concat

        ( newBattlerA1, newBattlerB1 ) =
            ( { battlerA | magicPoints = battlerA.magicPoints - realActionA.magicPointCost }, battlerB )
                |> Battler.applyEffects battlerAEffects
        
        ( newBattlerB2, newBattlerA2 ) =
            ( { newBattlerB1 | magicPoints = battlerB.magicPoints - realActionB.magicPointCost }, newBattlerA1 )
                |> Battler.applyEffects battlerBEffects
    in
    ( newBattlerA2, newBattlerB2 )

generateDungeonReward : Dungeon -> Random.Generator Reward
generateDungeonReward _ =
    Random.constant <|
        { experience = 5
        , gold = 5
        , abilityPoints = 5
        , items =
            [ ( Item.byId "potion", 3 )
            ]
        }