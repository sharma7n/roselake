module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
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

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- INIT

init : flags -> ( Model, Cmd Msg )
init _ =
    let
        initCharacterCreationSettings =
            { name = FormResult.FRBlank CharacterCreationError.MissingName
            , hairStyle = FormResult.FRBlank CharacterCreationError.MissingHairStyle
            , hairColor = FormResult.FRBlank CharacterCreationError.MissingHairColor
            , eyeColor = FormResult.FRBlank CharacterCreationError.MissingEyeColor
            , complexion = FormResult.FRBlank CharacterCreationError.MissingComplexion
            , height = FormResult.FRBlank CharacterCreationError.MissingHeight
            , build = FormResult.FRBlank CharacterCreationError.MissingBuild
            }
        
        initCharacterCreationModel =
            { settings = initCharacterCreationSettings
            }
        
        initModel =
            { phase = Phase.CharacterCreationPhase initCharacterCreationModel
            }
    in
    ( initModel, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    case model.phase of
        Phase.CharacterCreationPhase characterCreationModel ->
            viewCharacterCreationPhase characterCreationModel
        
        Phase.ScenePhase scene sceneModel ->
            viewScenePhase scene sceneModel

viewCharacterCreationPhase : CharacterCreationModel -> Html Msg
viewCharacterCreationPhase model =
    let
        settingToInfo : (a -> String) -> FormResult CharacterCreationError.Error a -> Html Msg
        settingToInfo aToString x =
            case x of
                FormResult.FRBlank _ ->
                    Html.text <| ""
                
                FormResult.FROk a ->
                    Html.text <| aToString a
                
                FormResult.FRErr e ->
                    Html.text <| CharacterCreationError.toString e
    in
    Html.div
        []
        [ Html.text "Create Character"
        , formList
            [ ( "Name"
              , Html.input [ Html.Events.onInput (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.NameSelection) ] []
              , settingToInfo (\a -> a) model.settings.name 
              )
            , ( "Hair Style"
              , radioButtons HairStyle.toString (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.HairStyleSelection) HairStyle.all model.settings.hairStyle
              , settingToInfo HairStyle.toString model.settings.hairStyle
              )
            , ( "Hair Color"
              , radioButtons HairColor.toString (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.HairColorSelection) HairColor.all model.settings.hairColor
              , settingToInfo HairColor.toString model.settings.hairColor
              )
            , ( "Eye Color"
              , radioButtons EyeColor.toString (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.EyeColorSelection) EyeColor.all model.settings.eyeColor
              , settingToInfo EyeColor.toString model.settings.eyeColor
              )
            , ( "Complexion"
              , radioButtons Complexion.toString (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.ComplexionSelection) Complexion.all model.settings.complexion
              , settingToInfo Complexion.toString model.settings.complexion
              )
            , ( "Height"
              , radioButtons Height.toString (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.HeightSelection) Height.all model.settings.height
              , settingToInfo Height.toString model.settings.height
              )
            , ( "Build"
              , radioButtons Build.toString (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.BuildSelection) Build.all model.settings.build
              , settingToInfo Build.toString model.settings.build
              )
            ]
        , Html.button [ Html.Events.onClick Msg.UserSelectedCharacterCreationConfirmation ] [ Html.text "Create" ]
        , Html.button [ Html.Events.onClick Msg.DevSelectedCharacterCreationConfirmation ] [ Html.text "Dev Create" ]
        ]

viewScenePhase : Scene -> SceneModel -> Html Msg
viewScenePhase scene sceneModel =
    Html.div
        []
        [ textList
            [ sceneModel.name
            , Avatar.description sceneModel.avatar
            , "G: " ++ String.fromInt sceneModel.gold
            , "LV: " ++ String.fromInt sceneModel.level
            , "EXP: " ++ String.fromInt sceneModel.experience ++ " / " ++ String.fromInt (levelUpExperience sceneModel.level)
            , "AP: " ++ String.fromInt sceneModel.freeAbilityPoints ++ " / " ++ String.fromInt sceneModel.totalAbilityPoints
            , "Satiety: " ++ String.fromInt sceneModel.satiety ++ " / " ++ String.fromInt sceneModel.maxSatiety
            , "HP: " ++ String.fromInt sceneModel.hitPoints ++ " / " ++ String.fromInt sceneModel.maxHitPoints
            , "MP: " ++ String.fromInt sceneModel.magicPoints ++ " / " ++ String.fromInt sceneModel.maxMagicPoints
            ]
        , viewInventory sceneModel.inventory
        , case scene of
            Scene.BattleMonsterScene _ _ ->
                Html.div
                    []
                    []
            
            Scene.ExploreDungeonScene _ _ ->
                Html.div
                    []
                    []
            
            _ ->
                 buttonList
                    [ ( "Player", Msg.UserSelectedPlayerScene )
                    , ( "Learn", Msg.UserSelectedLearnSelectScene )
                    , ( "Home", Msg.UserSelectedHomeScene )
                    , ( "Shop", Msg.UserSelectedShopSelectScene )
                    , ( "Town", Msg.NoOp )
                    , ( "Explore", Msg.UserSelectedExploreScene )
                    , ( "Battle", Msg.UserSelectedBattleScene )
                    ]
        , viewSceneModel scene sceneModel
        ]

viewInventory : Inventory -> Html Msg
viewInventory i =
    let
        itemQtyFn ( item, qty ) =
            Html.li
                []
                [ Html.text <| item.name ++ ": "
                , Html.text <| String.fromInt qty ++ " "
                , Html.button
                    [ Html.Events.onClick <| Msg.UserSelectedUseItem item ]
                    [ Html.text "Use" ]
                ]
        
        visibleItemQtys =
            i
                |> Inventory.toList
                |> List.filter (\(_, q) -> q > 0)
    in
    Html.div
        []
        [ Html.text "Inventory:"
        , Html.ul
            []
            ( List.map itemQtyFn visibleItemQtys )
        ]

textList : List String -> Html Msg
textList items =
    let
        itemFn item =
            Html.li
                []
                [ Html.text item ]
    in
    Html.ul
        []
        ( List.map itemFn items )

buttonList : List ( String, Msg ) -> Html Msg
buttonList items =
    let
        itemFn ( label, msg ) =
            Html.li
                []
                [ Html.button
                    [ Html.Events.onClick msg ]
                    [ Html.text label ]
                ]
    in
    Html.ul
        []
        ( List.map itemFn items )

formList : List ( String, Html Msg, Html Msg ) -> Html Msg
formList items =
    let
        itemFn ( label, form, info ) =
            Html.li
                []
                [ Html.text label
                , form
                , info
                ]
    in
    Html.ul
        []
        ( List.map itemFn items )

radioButtons : (a -> String) -> (a -> Msg) -> List a -> FormResult e a -> Html Msg
radioButtons toString toMsg items currentItem =
    let     
        itemFn item =
            Html.span
                []
                [ Html.input
                    [ Html.Attributes.type_ "radio"
                    , Html.Attributes.checked (FormResult.FROk item == currentItem)
                    , Html.Events.on "change" (Json.Decode.succeed <| toMsg item)
                    ]
                    []
                , Html.text <| toString item
                ]
    in
    Html.div
        []
        ( List.map itemFn items )
    

viewSceneModel : Scene -> SceneModel -> Html Msg
viewSceneModel scene sceneModel =
    case scene of
        Scene.PlayerScene ->
            Html.ul
                []
                [ textList
                    [ "ATK: " ++ String.fromInt sceneModel.attack
                    , "AGI: " ++ String.fromInt sceneModel.agility
                    ]
                ]
        
        Scene.LearnSelectScene ->
            learnTable Action.learnable sceneModel.actions
        
        Scene.HomeScene ->
            buttonList
                [ ( "Rest", Msg.UserSelectedHomeRest )
                ]
        
        Scene.ShopSelectScene ->
            shopTable
                [ Shop.byId "potionshop"
                ]
        
        Scene.ShopScene shop ->
            viewShopScene sceneModel shop
        
        Scene.ExploreScene ->
            dungeonTable
                [ Dungeon.byId "beginnerscave"
                ]
        
        Scene.ExploreDungeonScene delvePhase delve ->
            viewExploreDungeonScene sceneModel delvePhase delve
        
        Scene.BattleScene ->
            monsterTable
                [ Monster.byId "dummy"
                , Monster.byId "gremlin"
                ]
        
        Scene.BattleMonsterLoadingIntentScene monster ->
            Html.text <| monster.name ++ " is thinking..."
        
        Scene.BattleMonsterScene monster intent ->
            viewBattleMonsterScene sceneModel monster intent
        
        Scene.VictoryScene monster reward ->
            textList
                [ "You defeated " ++ monster.name ++ "!"
                , "Reward:"
                , "EXP: " ++ String.fromInt reward.experience
                ]
        
        Scene.GameOverScene ->
            textList
                [ "Defeated..."
                ]

learnTable : List Action -> List Action -> Html Msg
learnTable learnable learned =
    let
        toLearn =
            learnable
                |> List.filter (\l -> not (List.member l learned))
        
        learnFn learn =
            Html.li
                []
                [ Html.text <| learn.name ++ " | "
                , Html.text <| String.fromInt learn.learnCost ++ " "
                , Html.button
                    [ Html.Events.onClick <| Msg.UserSelectedLearnSkill learn ]
                    [ Html.text "Learn" ]
                ]
    in
    Html.ul
        []
        ( List.map learnFn toLearn )

viewExploreDungeonScene : SceneModel -> DelvePhase -> Delve -> Html Msg
viewExploreDungeonScene sceneModel delvePhase delve =
    Html.div
        []
        [ textList
            [ "Exploring: " ++ delve.dungeon.name
            , "Floor: " ++ String.fromInt delve.floor ++ " / " ++ String.fromInt delve.dungeon.depth
            ]
        , case delvePhase of
            DelvePhase.ExplorationPhase paths ->
                pathTable paths
            
            DelvePhase.ActionPhase scene ->
                case scene of
                    DungeonScene.BattleMonster monster intent ->
                        Html.div
                            []
                            [ textList
                                [ monster.name
                                , "HP: " ++ String.fromInt monster.hitPoints
                                , "Intent: " ++ intent.name
                                ]
                            , actionTable sceneModel.actions
                            ]
                    
                    DungeonScene.RestArea ->
                        buttonList
                            [ ( "Rest", Msg.UserSelectedRest )
                            , ( "Continue", Msg.UserSelectedContinueDungeon )
                            , ( "Exit Dungeon", Msg.UserSelectedExitDungeon )
                            ]
                    
                    DungeonScene.TrapDoor ->
                        Html.ul
                            []
                            [ Html.text "A trap door!"
                            , Html.button
                                [ Html.Events.onClick Msg.UserSelectedExitDungeon ]
                                [ Html.text "Exit Dungeon" ]
                            ]
                    
                    DungeonScene.LoadingGoal ->
                        Html.text "Loading goal..."
                    
                    DungeonScene.Goal reward ->
                        Html.ul
                            []
                            [ Html.text "Goal!"
                            , viewReward reward
                            , Html.button
                                [ Html.Events.onClick Msg.UserSelectedExitDungeon ]
                                [ Html.text "Exit Dungeon" ]
                            ]
                    
                    DungeonScene.Shop ->
                        Html.text "Loading shop..."
                    
                    DungeonScene.Treasure ->
                        Html.ul
                            []
                            [ Html.text "You find a treasure chest!"
                            , Html.button
                                [ Html.Events.onClick Msg.UserSelectedOpenChest ]
                                [ Html.text "Open" ]
                            , Html.button
                                [ Html.Events.onClick Msg.UserSelectedContinueDungeon ]
                                [ Html.text "Continue" ]
                            ]
                    
                    DungeonScene.Shopping shop ->
                        Html.ul
                            []
                            [ viewShopScene sceneModel shop
                            , Html.button
                                [ Html.Events.onClick Msg.UserSelectedContinueDungeon ]
                                [ Html.text "Continue" ]
                            ]
                    
                    _ ->
                        Html.ul
                            []
                            [ Html.text <| DungeonScene.toString scene
                            , Html.button
                                [ Html.Events.onClick <| Msg.UserSelectedContinueDungeon ]
                                [ Html.text "Continue" ]
                            ]
        ]

pathTable : List DungeonPath.Path -> Html Msg
pathTable paths =
    let
        pathFn path =
            Html.li
                []
                [ Html.text path.description
                , explainSceneDistribution path.sceneDistribution
                , Html.button
                    [ Html.Events.onClick <| Msg.UserSelectedDungeonPath path ]
                    [ Html.text "Go" ]
                ]
    in
    Html.ul
        []
        ( List.map pathFn paths )

viewReward : Reward -> Html Msg
viewReward r =
    let
        ( experienceReward, expVis ) =
            ( Html.li
                []
                [ Html.text <| "Got: " ++ String.fromInt r.experience ++ " EXP!" ]
            , r.experience > 0
            )
        
        ( goldReward, goldVis ) =
            ( Html.li
                []
                [ Html.text <| "Got: " ++ String.fromInt r.gold ++ " G!" ]
            , r.gold > 0
            )
        
        ( apReward, apVis ) =
            ( Html.li
                []
                [ Html.text <| "Got: " ++ String.fromInt r.abilityPoints ++ " AP!" ]
            , r.abilityPoints > 0
            )
        
        ( itemReward, itemVis ) =
            let
                relevant =
                    r.items
                        |> List.filter (\(_, q) -> q > 0)
            in
            ( let

                oneItemReward ( item, qty )=
                    Html.text <| "Got: " ++ String.fromInt qty ++ "x " ++ item.name ++ "!"
              in
              Html.li
                []
                ( List.map oneItemReward relevant )
            , List.length relevant > 0
            )
        
        display =
            [ ( experienceReward, expVis )
            , ( goldReward, goldVis )
            , ( apReward, apVis )
            , ( itemReward, itemVis )
            ]
                |> List.filter (\(_, vis) -> vis)
                |> List.map (\(d,_) -> d)            
    in
    Html.ul
        []
        display

shopTable : List Shop -> Html Msg
shopTable shops =
    let
        shopFn shop =
            Html.li
                []
                [ Html.text shop.name
                , Html.button
                    [ Html.Events.onClick <| Msg.UserSelectedShop shop ]
                    [ Html.text "Go" ]
                ]
    in
    Html.ul
        []
        ( List.map shopFn shops )
    
explainSceneDistribution : Distribution DungeonScene.Scene -> Html Msg
explainSceneDistribution d =
    let
        explainOneScene ( chance, scene ) =
            String.fromFloat chance ++ "% of " ++ DungeonScene.toString scene
    in
    textList (List.map explainOneScene (Distribution.toList d))

viewBattleMonsterScene : SceneModel -> Monster -> Action -> Html Msg
viewBattleMonsterScene sceneModel monster intent =
    Html.div
        []
        [ textList
            [ monster.name
            , "HP: " ++ String.fromInt monster.hitPoints
            , "Intent: " ++ intent.name
            ]
        , actionTable sceneModel.actions
        ]

actionTable : List Action -> Html Msg
actionTable actions =
    let
        actionFn action =
            Html.li
                []
                [ Html.text <| action.name ++ " "
                , Html.button
                    [ Html.Events.onClick <| Msg.UserSelectedBattleAction action ]
                    [ Html.text "Go" ]
                ]
    in
    Html.ul
        []
        ( List.map actionFn actions )

viewShopScene : SceneModel -> Shop -> Html Msg
viewShopScene sceneModel shop =
    let
        buyableFn b =
            Html.li
                []
                [ Html.text <| b.name ++ " | "
                , Html.text <| String.fromInt b.cost ++ " "
                , Html.button
                    [ Html.Events.onClick <| Msg.UserSelectedBuy b ]
                    [ Html.text "Buy" ]
                ]
    in
    Html.div
        []
        [ textList
            [ "Shop: " ++ shop.name
            ]
        , Html.ul
            []
            ( List.map buyableFn shop.stock )
        ]
levelUpExperience : Int -> Int
levelUpExperience n =
    10 * n * n

dungeonTable : List Dungeon -> Html Msg
dungeonTable dungeons =
    let
        dungeonFn dungeon =
            Html.span
                []
                [ Html.text <| dungeon.name
                , Html.button
                    [ Html.Events.onClick <| Msg.UserSelectedExploreDungeonScene dungeon ]
                    [ Html.text "Explore" ]
                ]
        
    in
    Html.ul
        []
        ( List.map dungeonFn dungeons )

monsterTable : List Monster -> Html Msg
monsterTable monsters =
    let
        monsterFn monster =
            Html.li
                []
                [ Html.text <| monster.name
                , Html.text <| " | HP: " ++ String.fromInt monster.hitPoints
                , Html.text <| " | EXP: " ++ String.fromInt monster.experience ++ " "
                , Html.button
                    [ Html.Events.onClick <| Msg.UserSelectedBattleMonsterScene monster ]
                    [ Html.text "Fight" ]
                ]
    in
    Html.ul
        []
        ( List.map monsterFn monsters )

-- UPDATE

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

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none