module View exposing
    ( view
    )

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
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

import SceneState exposing (SceneState)
import Requirement exposing (Requirement)
import Attribute exposing (Attribute)
import Battle exposing (Battle)
import Battler exposing (Battler)
import Target exposing (Target)
import Avatar exposing (Avatar)
import Delve exposing (Delve)
import Boss exposing (Boss)
import DelvePhase exposing (DelvePhase)
import DungeonPath
import DungeonScene
import Dungeon exposing (Dungeon)
import Action exposing (Action)
import Passive exposing (Passive)
import ActionState exposing (ActionState)
import Effect exposing (Effect)
import Monster exposing (Monster)
import MonsterTemplate exposing (MonsterTemplate)
import Reward exposing (Reward)
import Inventory exposing (Inventory)
import Object exposing (Object)
import Shop exposing (Shop)
import Item exposing (Item)
import Weapon exposing (Weapon)
import Status exposing (Status)
import Essentia exposing (Essentia)

import BossPhase exposing (BossPhase)
import BossState exposing (BossState)
import BossPath exposing (BossPath)
import BossScene exposing (BossScene)

import EssentiaContainer exposing (EssentiaContainer)
import StatusSet exposing (StatusSet)

import Scene exposing (Scene)
import Character exposing (Character)

import Phase exposing (Phase)

import Model exposing (Model)
import Msg exposing (Msg)

view : Model -> Html Msg
view model =
    case model.phase of
        Phase.CharacterCreationPhase characterCreationModel ->
            viewCharacterCreationPhase characterCreationModel
        
        Phase.ScenePhase scene sceneState character ->
            viewScenePhase scene sceneState character

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
            , ( "Starting Weapon"
              , radioButtons .name (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.StartingWeaponSelection) Weapon.listStarting model.settings.startingWeapon
              , settingToInfo .name model.settings.startingWeapon
              )
            , ( "Starting Essentia"
              , radioButtons .name (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.StartingEssentiaSelection) Essentia.listStarting model.settings.startingEssentia
              , settingToInfo .name model.settings.startingEssentia
              )
            ]
        , Html.ul
            []
            [ Html.li
                []
                [ Html.text <| "Attribute Points: " ++ String.fromInt model.attributePoints
                ]
            , attributeElement Attribute.Strength model.strength
            , attributeElement Attribute.Vitality model.vitality
            , attributeElement Attribute.Agility model.agility
            , attributeElement Attribute.Intellect model.intellect
            ]
        , Html.button [ Html.Events.onClick Msg.UserSelectedRandomCharacterCreation ] [ Html.text "Randomize" ]
        , Html.button [ Html.Events.onClick Msg.UserSelectedCharacterCreationConfirmation ] [ Html.text "Create" ]
        , Html.button [ Html.Events.onClick Msg.DevSelectedCharacterCreationConfirmation ] [ Html.text "Dev Create" ]
        ]

attributeElement : Attribute -> Int -> Html Msg
attributeElement attr value =
    Html.li
        []
        [ Html.text <| Attribute.toShortString attr ++ ": " ++ String.fromInt value
        , Html.button
            [ Html.Events.onClick <| Msg.UserSelectedModifyCharacterCreationAttribute attr (-1)
            ]
            [ Html.text "-"
            ]
        , Html.button
            [ Html.Events.onClick <| Msg.UserSelectedModifyCharacterCreationAttribute attr 1
            ]
            [ Html.text "+"
            ]
        ]

textListItem : String -> Html Msg
textListItem text =
    Html.li
        []
        [ Html.text text
        ]

statusSetListItem : StatusSet -> Html Msg
statusSetListItem statusSet =
    Html.li
        []
        [ viewStatusSet statusSet
        ]


viewScenePhase : Scene -> SceneState -> Character -> Html Msg
viewScenePhase scene sceneState character =
    Html.div
        []
        [ Html.ul
            []
            [ textListItem <| character.name
            , textListItem <| Avatar.description character.avatar
            , textListItem <| "G: " ++ String.fromInt character.gold
            , textListItem <| "LV: " ++ String.fromInt character.level
            , textListItem <| "EXP: " ++ String.fromInt character.experience ++ " / " ++ String.fromInt (levelUpExperience character.level)
            , textListItem <| "AP: " ++ String.fromInt character.freeAbilityPoints ++ " / " ++ String.fromInt character.totalAbilityPoints
            , statusSetListItem character.statusSet
            , textListItem <| "HP: " ++ String.fromInt character.hitPoints ++ " / " ++ String.fromInt (Battler.totalMaxHitPoints character)
            , textListItem <| "MP: " ++ String.fromInt character.magicPoints ++ " / " ++ String.fromInt character.maxMagicPoints
            ]
        , case scene of
            Scene.BattleMonster _ ->
                Html.div
                    []
                    []
            
            Scene.ExploreDungeon _ _ ->
                Html.div
                    []
                    []
            
            _ ->
                 buttonBar
                    [ ( "Player", Msg.UserSelectedScene Scene.Player )
                    , ( "Essentia", Msg.UserSelectedScene Scene.Essentia )
                    , ( "Learn", Msg.UserSelectedScene Scene.LearnSelect )
                    , ( "Equip", Msg.UserSelectedScene Scene.Equip )
                    , ( "Home", Msg.UserSelectedScene Scene.Home )
                    , ( "Shop", Msg.UserSelectedScene Scene.ShopSelect )
                    , ( "Town", Msg.UserSelectedScene Scene.Town )
                    , ( "Explore", Msg.UserSelectedScene Scene.Explore )
                    , ( "Boss", Msg.UserSelectedScene Scene.BossSelect )
                    ]
        , viewCharacter scene sceneState character
        , viewInventory character.inventory
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

buttonBar : List ( String, Msg ) -> Html Msg
buttonBar items =
    let
        itemFn ( label, msg ) =
            Html.span
                []
                [ Html.button
                    [ Html.Events.onClick msg ]
                    [ Html.text label ]
                ]
    in
    Html.div
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
    

viewCharacter : Scene -> SceneState -> Character -> Html Msg
viewCharacter scene sceneState character =
    case scene of
        Scene.Player ->
            Html.ul
                []
                [ Html.ol
                    []
                    [ Html.text "Attributes"
                    , textList
                        [ "STR: " ++ String.fromInt character.strength
                        , "VIT: " ++ String.fromInt character.vitality
                        , "AGI: " ++ String.fromInt character.agility
                        , "INT: " ++ String.fromInt character.intellect
                        ]
                    , Html.text "Equipment"
                    , textList
                        [ "Weapon: " ++ Maybe.withDefault " - " (Maybe.map .name character.equippedWeapon)
                        , "Armor: " ++ Maybe.withDefault " - " (Maybe.map .name character.equippedArmor)
                        ]
                    , Html.text "Passives"
                    , textList
                        ( character.learnedPassives
                            |> Set.toList
                            |> List.map Passive.byId
                            |> List.map .name
                        )
                    , Html.text "Actions"
                    , textList
                        ( character.learned
                            |> Set.toList
                            |> List.map Action.byId
                            |> List.map .name
                        )
                    ]
                ]
        
        Scene.Essentia ->
            viewEssentiaScene character.essentia character.essentiaContainer
        
        Scene.LearnSelect ->
            viewLearnScene character
        
        Scene.Equip ->
            let
                equippedWeaponElement =
                    case character.equippedWeapon of
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
                
                equippedArmorElement =
                    case character.equippedArmor of
                        Just armor ->
                            Html.div
                                []
                                [ Html.text <| "Armor: " ++ armor.name
                                , Html.button
                                    [ Html.Events.onClick <| Msg.UserSelectedUnEquipArmor armor ]
                                    [ Html.text "Un-equip" ]
                                ]
                        
                        Nothing ->
                            Html.div
                                []
                                [ Html.text <| "Armor: - "
                                ]
            in
            Html.div
                []
                [ Html.text "Equipped"
                , Html.ul
                    []
                    [ equippedWeaponElement
                    , equippedArmorElement
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
                    (List.map equipableFn (Inventory.listWeapons character.inventory))
                , let

                    equipableFn (w, q) =
                        Html.li
                            []
                            [ Html.text <| w.name ++ " (" ++ String.fromInt q ++ ")"
                            , Html.button
                                [ Html.Events.onClick <| Msg.UserSelectedEquipArmor w ]
                                [ Html.text "Equip" ]
                            ]
                  in
                  Html.ul
                    []
                    (List.map equipableFn (Inventory.listArmors character.inventory))
                ]
        
        Scene.Home ->
            buttonList
                [ ( "Rest", Msg.UserSelectedHomeRest )
                ]
        
        Scene.ShopSelect ->
            shopTable
                [ Shop.byId "potionshop"
                ]
        
        Scene.Shop shop ->
            viewShopScene character shop
        
        Scene.Town ->
            viewTownScene character
        
        Scene.OnyxTower ->
            viewOnyxTowerScene character
        
        Scene.Explore ->
            dungeonTable
                [ Dungeon.byId "beginnerscave"
                ]
        
        Scene.ExploreDungeon delvePhase delve ->
            viewExploreDungeon character delvePhase delve
        
        Scene.BattleSelect ->
            monsterTable
                [ MonsterTemplate.byId "dummy"
                , MonsterTemplate.byId "gremlin"
                , MonsterTemplate.byId "magic-eating-tortoise"
                , MonsterTemplate.byId "wyvern"
                ]
        
        Scene.BattleMonsterLoadingIntent ->
            case sceneState.maybeBattle of
                Just battle ->
                    Html.text <| battle.monster.name ++ " is thinking..."
                
                Nothing ->
                    textList []
        
        Scene.BattleMonster intent ->
            case sceneState.maybeBattle of
                Just battle ->
                    viewBattleMonster character battle intent
                
                Nothing ->
                    textList []
        
        Scene.VictoryLoading _ ->
            Html.text "Loading..."
        
        Scene.Victory monster reward ->
            textList
                [ "You defeated " ++ monster.name ++ "!"
                , "Reward:"
                , "EXP: " ++ String.fromInt reward.experience
                ]
        
        Scene.GameOver ->
            textList
                [ "Defeated..."
                ]
        
        Scene.Escaped ->
            textList
                [ "Escaped..."
                ]
        
        Scene.BossSelect ->
            viewBosses
                [ Boss.byId "leviathan"
                ]
        
        Scene.BossFight bossPhase ->
            case sceneState.ambient of
                SceneState.BossFight bossState ->
                    viewBossFight character bossPhase bossState
                
                _ ->
                    textList []
        
        Scene.Battle _ ->
            textList []

viewExploreDungeon : Character -> DelvePhase -> Delve -> Html Msg
viewExploreDungeon character delvePhase delve =
    Html.div
        []
        [ textList
            [ "Exploring: " ++ delve.dungeon.name
            , "Floor: " ++ String.fromInt delve.floor ++ " / " ++ String.fromInt delve.dungeon.depth
            ]
        , case delvePhase of
            DelvePhase.ExplorationPhase paths ->
                pathTable character paths
            
            DelvePhase.ActionPhase scene ->
                case scene of
                    DungeonScene.BattleMonster battle intent ->
                        viewBattleMonster character battle intent
                    
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
                            [ viewShopScene character shop
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

pathTable : Character -> List DungeonPath.Path -> Html Msg
pathTable m paths =
    let
        pathFn path =
            let
                can =
                    m
                        |> Character.satisfiesRequirements path.requirements
                
                onClick =
                    if can then
                        [ Html.Events.onClick <| Msg.UserSelectedDungeonPath path ]
                    else
                        [ Html.Attributes.disabled True ]
            in
            Html.li
                []
                [ Html.text path.description
                , viewRequirements path.requirements
                , explainSceneDistribution path.sceneDistribution
                , Html.button
                    onClick
                    [ Html.text "Go" ]
                ]
    in
    Html.ul
        []
        ( List.map pathFn paths )

viewRequirements : List Requirement -> Html Msg
viewRequirements l =
    let
        viewOneRequirement r =
            Html.li
                []
                [ Html.text <| "Requires: " ++ Requirement.toString r
                ]  
    in
    Html.ul
        []
        ( List.map viewOneRequirement l )

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

viewBattleMonster : Character -> Battle -> Action -> Html Msg
viewBattleMonster character battle intent =
    let
        monster = battle.monster
    in
    Html.div
        []
        [ textList
            [ "Round: " ++ String.fromInt battle.round
            ]
        , Html.ul
            []
            [ textListItem <| "Enemy"
            , textListItem <| monster.name
            , statusSetListItem <| monster.statusSet
            , textListItem <| "HP: " ++ String.fromInt monster.hitPoints ++ " / " ++ String.fromInt (Battler.totalMaxHitPoints monster)
            , textListItem <| "Intent: " ++ intent.name
            , textListItem <| "Block: " ++ String.fromInt monster.block
            ]
        , Html.ul
            []
            [ textListItem <| "Player"
            , textListItem <| character.name
            , statusSetListItem <| character.statusSet
            , textListItem <| "HP: " ++ String.fromInt character.hitPoints ++ " / " ++ String.fromInt (Battler.totalMaxHitPoints character)
            , textListItem <| "AP: " ++ String.fromInt character.actionPoints ++ " / " ++ String.fromInt character.maxActionPoints
            , textListItem <| "Block: " ++ String.fromInt character.block
            ]
        , actionTable character.actionPoints character.actionStates
        , Html.button
            [ Html.Events.onClick Msg.UserSelectedEndBattleTurn ]
            [ Html.text "End Turn" ]
        ]

actionTable : Int -> List ActionState -> Html Msg
actionTable actionPoints actions =
    let
        actionFn actionState =
            let
                buttonElement =
                    if ActionState.canUse actionPoints actionState then
                        Html.button
                            [ Html.Events.onClick <| Msg.UserSelectedBattleAction actionState.action ]
                            [ Html.text "Go" ]
                    else
                        Html.text ""
            in
            Html.li
                []
                [ Html.text <| actionState.action.name
                , Html.text " | "
                , Html.text <| "AP: " ++ String.fromInt actionState.action.actionPointCost
                , Html.text " | "
                , Html.text <| ActionState.stateToString actionState.state
                , Html.text " | "
                , buttonElement
                ]
    in
    Html.ul
        []
        ( List.map actionFn actions )

viewShopScene : Character -> Shop -> Html Msg
viewShopScene character shop =
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

monsterTable : List MonsterTemplate -> Html Msg
monsterTable monsters =
    let
        monsterFn monsterTemplate =
            Html.li
                []
                [ Html.text <| monsterTemplate.name
                , Html.text <| " | HP: " ++ String.fromInt monsterTemplate.hitPoints
                , Html.text <| " | EXP: " ++ String.fromInt monsterTemplate.experience ++ " "
                , Html.button
                    [ Html.Events.onClick <| Msg.UserSelectedMonsterTemplate monsterTemplate ]
                    [ Html.text "Fight" ]
                ]
    in
    Html.ul
        []
        ( List.map monsterFn monsters )

viewStatusSet : StatusSet -> Html Msg
viewStatusSet s =
    Html.div
        []
        [ Html.text "Statuses: "
        , viewStatusData (StatusSet.toList s)
        ]

viewStatusData : List StatusSet.Data -> Html Msg
viewStatusData data =
    let
        viewOneStatusData datum =
            Html.li
                []
                [ Html.text <| Status.toString datum.status ++ ": " ++ String.fromInt datum.stacks
                ]
    in
    Html.ul
        []
        ( List.map viewOneStatusData data )

viewEssentiaScene : List Essentia -> EssentiaContainer -> Html Msg
viewEssentiaScene e c =
    let
        containerSlotFn ( slotIndex, slot ) =
            case slot of
                Just essentia ->
                    Html.li
                        []
                        [ Html.text <| "Slot " ++ EssentiaContainer.indexToString slotIndex ++ ": " ++ essentia.name
                        , Html.button
                            [ Html.Events.onClick <| Msg.UserSelectedUnEquipEssentia slotIndex essentia ]
                            [ Html.text "Un-Equip" ]
                        ]
                
                Nothing ->
                    Html.li
                        []
                        [ Html.text <| "Slot " ++ EssentiaContainer.indexToString slotIndex ++ ": -"
                        ]

        equipEssentiaButtonFn esn listIdx slotIndex =
            Html.button
                [ Html.Events.onClick <| Msg.UserSelectedEquipEssentia slotIndex listIdx esn ]
                [ Html.text <| "Equip in Slot " ++ EssentiaContainer.indexToString slotIndex ]
        essentiaFn listIdx esn =
            Html.div
                []
                ( Html.text esn.name
                :: ( List.map (equipEssentiaButtonFn esn listIdx) EssentiaContainer.listIndices )
                )
    in
    Html.ul
        []
        [ Html.text "Equipped"
        , Html.ul
            []
            ( List.map containerSlotFn (EssentiaContainer.toList c))
        , Html.text "Equipable"
        , Html.ul
            []
            ( List.indexedMap essentiaFn e )
        ]

viewLearnScene : Character -> Html Msg
viewLearnScene m =
    let
        learnableEssentia =
            EssentiaContainer.listIndices
                |> List.map (\idx -> EssentiaContainer.getSlot idx m.essentiaContainer)
                |> List.filterMap (\a -> a)
        
        viewLearnOneAction a =
            let
                learnElement =
                    if Set.member a.id m.learned then
                        Html.text "Learned"
                    else
                        if a.learnCost <= m.freeAbilityPoints then
                            Html.button
                                [ Html.Events.onClick <| Msg.UserSelectedLearnSkill a
                                ]
                                [ Html.text <| "Learn (" ++ String.fromInt a.learnCost ++ " AP)"
                                ]
                        else
                            Html.button
                                [ Html.Attributes.disabled True
                                ]
                                [ Html.text <| "Learn (" ++ String.fromInt a.learnCost ++ " AP) (Insufficient)"
                                ]
            in
            Html.div
                []
                [ Html.text a.name
                , learnElement
                ]
        
        viewLearnOnePassive p =
            let
                learnElement =
                    if Set.member p.id m.learnedPassives then
                        Html.text "Learned"
                    else
                        if p.learnCost <= m.freeAbilityPoints then
                            Html.button
                                [ Html.Events.onClick <| Msg.UserSelectedLearnPassive p
                                ]
                                [ Html.text <| "Learn (" ++ String.fromInt p.learnCost ++ " AP)"
                                ]
                        else
                            Html.button
                                [ Html.Attributes.disabled True
                                ]
                                [ Html.text <| "Learn (" ++ String.fromInt p.learnCost ++ " AP) (Insufficient)"
                                ]
            in
            Html.div
                []
                [ Html.text p.name
                , learnElement
                ]
        
        viewLearnOneEssentia e =
            Html.div
                []
                [ Html.text e.name
                , Html.li
                    []
                    ( List.map viewLearnOneAction e.actions )
                , Html.li
                    []
                    ( List.map viewLearnOnePassive e.passives )
                ]
    in
    Html.span
        []
        ( List.map viewLearnOneEssentia learnableEssentia )

viewTownScene : Character -> Html Msg
viewTownScene m =
    Html.ul
        []
        [ Html.li
            []
            [ Html.text "Onyx Tower"
            , Html.button
                [ Html.Events.onClick <| Msg.UserSelectedOnyxTower
                ]
                [ Html.text "Go"
                ]
            ]
        , Html.li
            []
            [ Html.text "Potion Shop"
            , Html.button
                [ Html.Events.onClick <| Msg.UserSelectedShop (Shop.byId "potionshop")
                ]
                [ Html.text "Go"
                ]
            ]
        ]

viewOnyxTowerScene : Character -> Html Msg
viewOnyxTowerScene m =
    Html.ul
        []
        [ Html.li
            []
            [ Html.text "It's the Onyx Tower"
            ]
        ]

viewBosses : List Boss -> Html Msg
viewBosses bosses =
    let
        viewOneBoss boss =
            Html.li
                []
                [ Html.text boss.name
                , Html.button
                    [ Html.Events.onClick <| Msg.UserSelectedBossFight boss
                    ]
                    [ Html.text "Go"
                    ]
                ]
    in
    Html.ul
        []
        ( List.map viewOneBoss bosses )

viewBossFight : Character -> BossPhase -> BossState -> Html Msg
viewBossFight character phase state =
    Html.div
        []
        [ if state.boss.showBoss then
            textList
                [ "Boss: " ++ state.monster.name
                , "HP: " ++ String.fromInt state.monster.hitPoints ++ " / " ++ String.fromInt state.monster.maxHitPoints
                ]
          else
            Html.div [] []
        , case phase of
            BossPhase.ExplorationPhase paths ->
                bossPathTable paths
            
            BossPhase.ActionPhase scene ->
                case scene of
                    BossScene.BattleBoss ->
                        Html.div [] []
                    
                    BossScene.BattleBossLoadingIntent _ ->
                        Html.div [] []
                    
                    BossScene.BattleBossOngoing battle intent ->
                        viewBattleMonster character battle intent
                    
                    _ ->
                        Html.ul
                            []
                            [ Html.text <| BossScene.toString scene
                            , Html.button
                                [ Html.Events.onClick <| Msg.UserSelectedContinueBossFight state.boss ]
                                [ Html.text "Continue" ]
                            ]
        ]

bossPathTable : List BossPath -> Html Msg
bossPathTable paths =
    let
        pathFn path =
            Html.li
                []
                [ Html.text path.description
                , explainBossSceneDistribution path.sceneDistribution
                , Html.button
                    [ Html.Events.onClick <| Msg.UserSelectedBossPath path ]
                    [ Html.text "Go" ]
                ]
    in
    Html.ul
        []
        ( List.map pathFn paths )

explainBossSceneDistribution : Distribution BossScene -> Html Msg
explainBossSceneDistribution d =
    let
        explainOneScene ( chance, scene ) =
            String.fromFloat chance ++ "% of " ++ BossScene.toString scene
    in
    textList (List.map explainOneScene (Distribution.toList d))