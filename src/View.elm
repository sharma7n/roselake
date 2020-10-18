module View exposing
    ( view
    )

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
                    , ( "Equip", Msg.UserSelectedEquipScene )
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
                |> Inventory.listItems
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
        
        Scene.EquipScene ->
            let
                equippedWeaponElement =
                    case sceneModel.equippedWeapon of
                        Just weapon ->
                            Html.div
                                []
                                [ Html.text <| "Weapon: " ++ weapon.name
                                , Html.button
                                    [ Html.Events.onClick <| Msg.UserSelectedUnEquipWeapon weapon ]
                                    [ Html.text "Un-equip" ]
                                ]
                        
                        Nothing ->
                            Html.div
                                []
                                [ Html.text <| "Weapon: - "
                                ]
            in
            Html.div
                []
                [ Html.text "Equipped"
                , Html.ul
                    []
                    [ equippedWeaponElement
                    ]
                , Html.text "Equipable"
                , let

                    equipableFn (w, q) =
                        Html.li
                            []
                            [ Html.text <| w.name ++ " (" ++ String.fromInt q ++ ")"
                            , Html.button
                                [ Html.Events.onClick <| Msg.UserSelectedEquipWeapon w ]
                                [ Html.text "Equip" ]
                            ]
                  in
                  Html.ul
                    []
                    (List.map equipableFn (Inventory.listWeapons sceneModel.inventory))
                ]
        
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
        
        Scene.VictoryLoadingScene monster ->
            Html.text "Loading..."
        
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
                        viewBattleMonsterScene sceneModel monster intent
                    
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
        , Html.text <| "AP: " ++ String.fromInt sceneModel.actionPoints ++ " / " ++ String.fromInt sceneModel.maxActionPoints
        , actionTable sceneModel.actionPoints sceneModel.actions
        , Html.button
            [ Html.Events.onClick Msg.UserSelectedEndBattleTurn ]
            [ Html.text "End Turn" ]
        ]

actionTable : Int -> List Action -> Html Msg
actionTable actionPoints actions =
    let
        availableActions =
            actions
                |> List.filter (\a -> a.actionPointCost <= actionPoints)
        
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
        ( List.map actionFn availableActions )

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