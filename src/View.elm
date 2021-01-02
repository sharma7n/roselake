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

import SceneState exposing (SceneState)
import Requirement exposing (Requirement)
import Attribute exposing (Attribute)
import Battle exposing (Battle)
import Battler exposing (Battler)
import Target exposing (Target)
import Avatar exposing (Avatar)
import Delve exposing (Delve)
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

import EssentiaContainer exposing (EssentiaContainer)
import StatusSet exposing (StatusSet)

import Scene exposing (Scene)
import Character exposing (Character)

import Phase exposing (Phase)

import Model exposing (Model)
import Msg exposing (Msg)

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Keyed
import Element.Lazy
import Element.Region

import Palette

view : Model -> Html Msg
view model =
    Element.layout
        [ Element.Font.size 14
        , Element.Font.family
            [ Element.Font.typeface "Verdana"
            ]
        , Element.padding 10
        ]
        ( viewModel model )

viewModel : Model -> Element Msg
viewModel model =
    case model.phase of
        Phase.CharacterCreationPhase characterCreationModel ->
            viewCharacterCreationPhase characterCreationModel
        
        Phase.ScenePhase scene sceneState character ->
            viewScenePhase scene sceneState character

viewCharacterCreationPhase : CharacterCreationModel -> Element Msg
viewCharacterCreationPhase model =
    let
        settingToInfo : (a -> String) -> FormResult CharacterCreationError.Error a -> Element Msg
        settingToInfo aToString x =
            case x of
                FormResult.FRBlank _ ->
                    Element.text <| ""
                
                FormResult.FROk a ->
                    Element.text <| aToString a
                
                FormResult.FRErr e ->
                    Element.text <| CharacterCreationError.toString e
    in
    Element.column
        [ Element.spacing 10
        ]
        [ Element.el
            [ Element.Font.heavy
            ]
            ( Element.text "Create Character" )
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 10
            ]
            [ Element.row
                [ Element.width Element.fill
                , Element.Background.color <| Palette.lightBlue
                , Element.padding 10
                ]
                [ Element.el
                    [ Element.alignLeft
                    ]
                    ( Element.text "Name" )
                , Element.el
                    [ Element.alignRight
                    ]
                    ( Element.Input.text
                        []
                        { onChange = (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.NameSelection)
                        , text = FormResult.fold (\e -> "") (\a -> a) (\e -> "") model.settings.name
                        , placeholder = Nothing
                        , label =
                            Element.Input.labelHidden "Name"
                        }
                    )
                ]
            , radioButtons
                "Hair Style"
                HairStyle.toString 
                (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.HairStyleSelection) 
                HairStyle.all 
                model.settings.hairStyle
            , radioButtons
                "Hair Color"
                HairColor.toString 
                (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.HairColorSelection) 
                HairColor.all 
                model.settings.hairColor
            , radioButtons
                "Eye Color"
                EyeColor.toString 
                (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.EyeColorSelection) 
                EyeColor.all 
                model.settings.eyeColor
            , radioButtons
                "Complexion"
                Complexion.toString 
                (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.ComplexionSelection) 
                Complexion.all 
                model.settings.complexion
            , radioButtons
                "Height"
                Height.toString 
                (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.HeightSelection) 
                Height.all 
                model.settings.height
            , radioButtons
                "Build"
                Build.toString 
                (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.BuildSelection) 
                Build.all 
                model.settings.build
            , radioButtons 
                "Starting Weapon"
                .name 
                (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.StartingWeaponSelection) 
                Weapon.listStarting 
                model.settings.startingWeapon
            , radioButtons
                "Starting Essentia"
                .name 
                (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.StartingEssentiaSelection) 
                Essentia.listStarting 
                model.settings.startingEssentia
            ]
        , Element.column
            [ Element.padding 10
            , Element.spacing 10
            ]
            [ Element.text <| "Attribute Points: " ++ String.fromInt model.attributePoints
            , attributeElement Attribute.Strength model.strength
            , attributeElement Attribute.Vitality model.vitality
            , attributeElement Attribute.Agility model.agility
            , attributeElement Attribute.Intellect model.intellect
            ]
        , Element.row
            [ Element.spacing 10
            ]
            [ button "Randomize" Msg.UserSelectedRandomCharacterCreation
            , button "Create" Msg.UserSelectedCharacterCreationConfirmation
            , button "Dev Create" Msg.DevSelectedCharacterCreationConfirmation
            ]
        ]

attributeElement : Attribute -> Int -> Element Msg
attributeElement attr value =
    Element.row
        [ Element.spacing 10
        , Element.width <| Element.px 200
        ]
        [ Element.el
            [ Element.alignLeft
            ]
            ( coloredButton Palette.lightRed "-" (Msg.UserSelectedModifyCharacterCreationAttribute attr (-1)) )
        , Element.el
            [ Element.centerX
            ]
            ( Element.text <| Attribute.toShortString attr ++ ": " ++ String.fromInt value )
        , Element.el
            [ Element.alignRight
            ]
            ( coloredButton Palette.lightGreen "+" (Msg.UserSelectedModifyCharacterCreationAttribute attr 1) )
        ]

textListItem : String -> Element Msg
textListItem text =
    Element.text text

statusSetListItem : StatusSet -> Element Msg
statusSetListItem statusSet =
    Element.column
        []
        [ viewStatusSet statusSet
        ]


viewScenePhase : Scene -> SceneState -> Character -> Element Msg
viewScenePhase scene sceneState character =
    Element.column
        []
        [ Element.row
            []
            [ Element.column
                []
                [ Element.text <| character.name
                , Element.text <| "G: " ++ String.fromInt character.gold
                , Element.text <| "LV: " ++ String.fromInt character.level
                , Element.text <| "EXP: " ++ String.fromInt character.experience ++ " / " ++ String.fromInt (levelUpExperience character.level)
                , Element.text <| "AP: " ++ String.fromInt character.freeAbilityPoints ++ " / " ++ String.fromInt character.totalAbilityPoints
                , Element.text <| "HP: " ++ String.fromInt character.hitPoints ++ " / " ++ String.fromInt (Battler.totalMaxHitPoints character)
                , Element.text <| "MP: " ++ String.fromInt character.magicPoints ++ " / " ++ String.fromInt character.maxMagicPoints
                ]
            , Element.column
                []
                [ Element.text <| "Build: " ++ Height.toString character.avatar.height ++ " & " ++ Build.toString character.avatar.build
                , Element.text <| "Complexion: " ++ Complexion.toString character.avatar.complexion
                , Element.text <| "Hair: " ++ HairStyle.toString character.avatar.hairStyle ++ " & " ++ HairColor.toString character.avatar.hairColor
                , Element.text <| "Eye Color: " ++ EyeColor.toString character.avatar.eyeColor
                ]
            , viewStatusSet character.statusSet
            ]
        , case scene of
            Scene.BattleMonster ->
                Element.none
            
            Scene.ExploreDungeon ->
                Element.none
            
            _ ->
                 buttonBar
                    [ ( "Player", Msg.UserSelectedScene Scene.Player )
                    , ( "Essentia", Msg.UserSelectedScene Scene.Essentia )
                    , ( "Learn", Msg.UserSelectedScene Scene.LearnSelect )
                    , ( "Equip", Msg.UserSelectedScene Scene.Equip )
                    , ( "Town", Msg.UserSelectedScene Scene.Town )
                    , ( "Explore", Msg.UserSelectedScene Scene.DungeonSelect )
                    ]
        , viewCharacter scene sceneState character
        , viewInventory character.inventory
        ]

viewInventory : Inventory -> Element Msg
viewInventory i =
    let
        itemQtyFn ( item, qty ) =
            Element.column
                []
                [ Element.text <| item.name ++ ": "
                , Element.text <| String.fromInt qty ++ " "
                , button "Use" (Msg.UserSelectedUseItem item)
                ]
        
        visibleItemQtys =
            i
                |> Inventory.listItems
                |> List.filter (\(_, q) -> q > 0)
    in
    Element.column
        []
        [ Element.text "Inventory:"
        , Element.column
            []
            ( List.map itemQtyFn visibleItemQtys )
        ]

textList : List String -> Element Msg
textList items =
    let
        itemFn item =
            Element.text item
    in
    Element.column
        []
        ( List.map itemFn items )

buttonList : List ( String, Msg ) -> Element Msg
buttonList items =
    let
        itemFn ( label, msg ) =
            button label msg
    in
    Element.column
        []
        ( List.map itemFn items )

buttonBar : List ( String, Msg ) -> Element Msg
buttonBar items =
    let
        itemFn ( label, msg ) =
            button label msg
    in
    Element.row
        [ Element.spacing 10
        ]
        ( List.map itemFn items )

radioButtons : String -> (a -> String) -> (a -> Msg) -> List a -> FormResult e a -> Element Msg
radioButtons labelText toString toMsg items currentItem =
    Element.row
        [ Element.width Element.fill
        , Element.Background.color <| Palette.lightBlue
        , Element.padding 10
        , Element.spacing 100
        ]
        [ Element.el
            [ Element.alignLeft
            ]
            ( Element.text labelText )
        , Element.el
            [ Element.alignRight
            ]
            ( Element.Input.radioRow
                [ Element.spacing 10
                ]
                { onChange = toMsg
                , options =
                    items
                        |> List.map (\i ->
                            Element.Input.option
                                i
                                ( Element.text <| toString i )
                        )
                , selected = FormResult.toMaybe currentItem
                , label =
                    Element.Input.labelHidden
                        ( currentItem
                            |> FormResult.toMaybe
                            |> Maybe.map toString
                            |> Maybe.withDefault ""
                        )
                }
            )
        ]
    

viewCharacter : Scene -> SceneState -> Character -> Element Msg
viewCharacter scene sceneState character =
    case scene of
        Scene.Player ->
            Element.column
                []
                [ Element.column
                    []
                    [ Element.text "Attributes"
                    , textList
                        [ "STR: " ++ String.fromInt character.strength
                        , "VIT: " ++ String.fromInt character.vitality
                        , "AGI: " ++ String.fromInt character.agility
                        , "INT: " ++ String.fromInt character.intellect
                        ]
                    , Element.text "Equipment"
                    , textList
                        [ "Weapon: " ++ Maybe.withDefault " - " (Maybe.map .name character.equippedWeapon)
                        , "Armor: " ++ Maybe.withDefault " - " (Maybe.map .name character.equippedArmor)
                        ]
                    , Element.text "Passives"
                    , textList
                        ( character.learnedPassives
                            |> Set.toList
                            |> List.map Passive.byId
                            |> List.map .name
                        )
                    , Element.text "Actions"
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
                            Element.column
                                []
                                [ Element.text <| "Weapon: " ++ weapon.name
                                , button "Un-Equip" (Msg.UserSelectedUnEquipWeapon weapon)
                                ]
                        
                        Nothing ->
                            Element.column
                                []
                                [ Element.text <| "Weapon: - "
                                ]
                
                equippedArmorElement =
                    case character.equippedArmor of
                        Just armor ->
                            Element.column
                                []
                                [ Element.text <| "Armor: " ++ armor.name
                                , button "Un-Equip" (Msg.UserSelectedUnEquipArmor armor)
                                ]
                        
                        Nothing ->
                            Element.text <| "Armor: - "
            in
            Element.column
                []
                [ Element.text "Equipped"
                , Element.column
                    []
                    [ equippedWeaponElement
                    , equippedArmorElement
                    ]
                , Element.text "Equipable"
                , let

                    equipableFn (w, q) =
                        Element.column
                            []
                            [ Element.text <| w.name ++ " (" ++ String.fromInt q ++ ")"
                            , button "Equip" (Msg.UserSelectedEquipWeapon w)
                            ]
                  in
                  Element.column
                    []
                    (List.map equipableFn (Inventory.listWeapons character.inventory))
                , let

                    equipableFn (w, q) =
                        Element.column
                            []
                            [ Element.text <| w.name ++ " (" ++ String.fromInt q ++ ")"
                            , button "Equip" (Msg.UserSelectedEquipArmor w)
                            ]
                  in
                  Element.column
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
        
        Scene.Shop ->
            case sceneState.maybeShop of
                Just shop ->
                    viewShopScene character shop
                
                _ ->
                    textList []
        
        Scene.Town ->
            viewTownScene character
        
        Scene.DungeonSelect ->
            dungeonTable
                [ Dungeon.byId "beginnerscave"
                ]
        
        Scene.ExploreDungeon ->
            case sceneState.ambient of
                SceneState.Delving delvePhase delve ->
                    viewExploreDungeon character delvePhase delve
                
                _ ->
                    textList []
        
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
                    Element.text <| battle.monster.name ++ " is thinking..."
                
                Nothing ->
                    textList []
        
        Scene.BattleMonster->
            case ( sceneState.maybeBattle, sceneState.maybeMonsterAction ) of
                ( Just battle, Just monsterAction ) ->
                    viewBattleMonster character battle monsterAction
                
                _ ->
                    textList []
        
        Scene.VictoryLoading ->
            Element.text "Loading..."
        
        Scene.Victory ->
            let
                f battle reward =
                    let
                        victoryMessage =
                            textList
                                [ "You defeated " ++ battle.monster.name ++ "!"
                                , "Reward:"
                                , "EXP: " ++ String.fromInt reward.experience
                                ]
                    in
                    case sceneState.ambient of
                        SceneState.Rest ->
                            Element.column
                                []
                                [ victoryMessage
                                ]
                        
                        SceneState.Delving _ _ ->
                            Element.column
                                []
                                [ victoryMessage
                                , continueButton
                                ]
            in
            Just f
                |> Maybe.Extra.andMap sceneState.maybeBattle
                |> Maybe.Extra.andMap sceneState.maybeReward
                |> Maybe.withDefault (textList [])
        
        Scene.GameOver ->
            textList
                [ "Defeated..."
                ]
        
        Scene.Escaped ->
            textList
                [ "Escaped..."
                ]

viewExploreDungeon : Character -> DelvePhase -> Delve -> Element Msg
viewExploreDungeon character delvePhase delve =
    Element.column
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
                        Element.column
                            []
                            [ Element.text "A trap door!"
                            , exitButtonDungeon
                            ]
                    
                    DungeonScene.LoadingGoal ->
                        Element.text "Loading goal..."
                    
                    DungeonScene.Goal reward ->
                        Element.column
                            []
                            [ Element.text "Goal!"
                            , viewReward reward
                            , exitButtonDungeon
                            ]
                    
                    DungeonScene.Shop ->
                        Element.text "Loading shop..."
                    
                    DungeonScene.Treasure ->
                        Element.column
                            []
                            [ Element.text "You find a treasure chest!"
                            , button "Open" Msg.UserSelectedOpenChest
                            , continueButton
                            ]
                    
                    DungeonScene.Shopping shop ->
                        Element.column
                            []
                            [ viewShopScene character shop
                            , continueButton
                            ]
                    
                    _ ->
                        Element.column
                            []
                            [ Element.text <| DungeonScene.toString scene
                            , continueButton
                            ]
        ]
continueButton : Element Msg
continueButton =
    button "Continue" Msg.UserSelectedContinueDungeon

exitButtonDungeon : Element Msg
exitButtonDungeon =
    button "Exit Dungeon" Msg.UserSelectedExitDungeon

pathTable : Character -> List DungeonPath.Path -> Element Msg
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
            Element.Input.button
                []
                { onPress =
                    if can then
                        Just <| Msg.UserSelectedDungeonPath path
                    else
                        Nothing
                , label =
                    Element.column
                        []
                        [ Element.text path.description
                        , viewRequirements path.requirements
                        , explainSceneDistribution path.sceneDistribution
                        ]
                }
    in
    Element.column
        []
        ( List.map pathFn paths )

viewRequirements : List Requirement -> Element Msg
viewRequirements l =
    let
        viewOneRequirement r =
            Element.text <| "Requires: " ++ Requirement.toString r
    in
    Element.column
        []
        ( List.map viewOneRequirement l )

viewReward : Reward -> Element Msg
viewReward r =
    let
        ( experienceReward, expVis ) =
            ( Element.text <| "Got: " ++ String.fromInt r.experience ++ " EXP!"
            , r.experience > 0
            )
        
        ( goldReward, goldVis ) =
            ( Element.text <| "Got: " ++ String.fromInt r.gold ++ " G!"
            , r.gold > 0
            )
        
        ( apReward, apVis ) =
            ( Element.text <| "Got: " ++ String.fromInt r.abilityPoints ++ " AP!"
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
                    Element.text <| "Got: " ++ String.fromInt qty ++ "x " ++ item.name ++ "!"
              in
              Element.column
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
    Element.column
        []
        display

shopTable : List Shop -> Element Msg
shopTable shops =
    let
        shopFn shop =
            Element.column
                []
                [ Element.text shop.name
                , button "Go" (Msg.UserSelectedShop shop)
                ]
    in
    Element.column
        []
        ( List.map shopFn shops )
    
explainSceneDistribution : Distribution DungeonScene.Scene -> Element Msg
explainSceneDistribution d =
    let
        explainOneScene ( chance, scene ) =
            String.fromFloat chance ++ "% of " ++ DungeonScene.toString scene
    in
    textList (List.map explainOneScene (Distribution.toList d))

viewBattleMonster : Character -> Battle -> Action -> Element Msg
viewBattleMonster character battle intent =
    let
        monster = battle.monster
    in
    Element.column
        []
        [ textList
            [ "Round: " ++ String.fromInt battle.round
            ]
        , Element.column
            []
            [ Element.text <| "Enemy"
            , Element.text <| monster.name
            , viewStatusSet <| monster.statusSet
            , Element.text <| "HP: " ++ String.fromInt monster.hitPoints ++ " / " ++ String.fromInt (Battler.totalMaxHitPoints monster)
            , Element.text <| "Intent: " ++ intent.name
            , Element.text <| "Block: " ++ String.fromInt monster.block
            ]
        , Element.column
            []
            [ Element.text <| "Player"
            , Element.text <| character.name
            , viewStatusSet <| character.statusSet
            , Element.text <| "HP: " ++ String.fromInt character.hitPoints ++ " / " ++ String.fromInt (Battler.totalMaxHitPoints character)
            , Element.text <| "AP: " ++ String.fromInt character.actionPoints ++ " / " ++ String.fromInt character.maxActionPoints
            , Element.text <| "Block: " ++ String.fromInt character.block
            ]
        , actionTable character.actionPoints character.actionStates
        , button "End Turn" (Msg.UserSelectedEndBattleTurn)
        ]

actionTable : Int -> List ActionState -> Element Msg
actionTable actionPoints actions =
    let
        actionFn actionState =
            let
                buttonElement =
                    if ActionState.canUse actionPoints actionState then
                        button "Go" (Msg.UserSelectedBattleAction actionState.action)
                    else
                        Element.text ""
            in
            Element.column
                []
                [ Element.text <| actionState.action.name
                , Element.text " | "
                , Element.text <| "AP: " ++ String.fromInt actionState.action.actionPointCost
                , Element.text " | "
                , Element.text <| ActionState.stateToString actionState.state
                , Element.text " | "
                , buttonElement
                ]
    in
    Element.column
        []
        ( List.map actionFn actions )

viewShopScene : Character -> Shop -> Element Msg
viewShopScene character shop =
    let
        buyableFn b =
            Element.column
                []
                [ Element.text <| b.name ++ " | "
                , Element.text <| String.fromInt b.cost ++ " "
                , button "Buy" (Msg.UserSelectedBuy b)
                ]
    in
    Element.column
        []
        [ textList
            [ "Shop: " ++ shop.name
            ]
        , Element.column
            []
            ( List.map buyableFn shop.stock )
        ]
levelUpExperience : Int -> Int
levelUpExperience n =
    10 * n * n

dungeonTable : List Dungeon -> Element Msg
dungeonTable dungeons =
    let
        dungeonFn dungeon =
            Element.row
                []
                [ Element.text <| dungeon.name
                , button "Explore" (Msg.UserSelectedExploreDungeonScene dungeon)
                ]
        
    in
    Element.column
        []
        ( List.map dungeonFn dungeons )

monsterTable : List MonsterTemplate -> Element Msg
monsterTable monsters =
    let
        monsterFn monsterTemplate =
            Element.column
                []
                [ Element.text <| monsterTemplate.name
                , Element.text <| " | HP: " ++ String.fromInt monsterTemplate.hitPoints
                , Element.text <| " | EXP: " ++ String.fromInt monsterTemplate.experience ++ " "
                , button "Fight" (Msg.UserSelectedMonsterTemplate monsterTemplate)
                ]
    in
    Element.column
        []
        ( List.map monsterFn monsters )

viewStatusSet : StatusSet -> Element Msg
viewStatusSet s =
    viewStatusData (StatusSet.toList s)

viewStatusData : List StatusSet.Data -> Element Msg
viewStatusData data =
    let
        viewOneStatusData datum =
            Element.text <| Status.toString datum.status ++ ": " ++ String.fromInt datum.stacks
    in
    Element.column
        []
        ( List.map viewOneStatusData data )

viewEssentiaScene : List Essentia -> EssentiaContainer -> Element Msg
viewEssentiaScene e c =
    let
        containerSlotFn ( slotIndex, slot ) =
            case slot of
                Just essentia ->
                    Element.column
                        []
                        [ Element.text <| "Slot " ++ EssentiaContainer.indexToString slotIndex ++ ": " ++ essentia.name
                        , button "Un-Equip" (Msg.UserSelectedUnEquipEssentia slotIndex essentia)
                        ]
                
                Nothing ->
                    Element.column
                        []
                        [ Element.text <| "Slot " ++ EssentiaContainer.indexToString slotIndex ++ ": -"
                        ]

        equipEssentiaButtonFn esn listIdx slotIndex =
            button ("Equip in Slot " ++ EssentiaContainer.indexToString slotIndex) (Msg.UserSelectedEquipEssentia slotIndex listIdx esn)

        essentiaFn listIdx esn =
           Element.column
                []
                ( Element.text esn.name
                :: ( List.map (equipEssentiaButtonFn esn listIdx) EssentiaContainer.listIndices )
                )
    in
    Element.column
        []
        [ Element.text "Equipped"
        , Element.column
            []
            ( List.map containerSlotFn (EssentiaContainer.toList c))
        , Element.text "Equipable"
        , Element.column
            []
            ( List.indexedMap essentiaFn e )
        ]

viewLearnScene : Character -> Element Msg
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
                        Element.text "Learned"
                    else
                        if a.learnCost <= m.freeAbilityPoints then
                            button ("Learn (" ++ String.fromInt a.learnCost ++ " AP)") (Msg.UserSelectedLearnSkill a)
                        else
                            disabledButton ("Learn (" ++ String.fromInt a.learnCost ++ " AP) (Insufficient)")
            in
            Element.column
                []
                [ Element.text a.name
                , learnElement
                ]
        
        viewLearnOnePassive p =
            let
                learnElement =
                    if Set.member p.id m.learnedPassives then
                        Element.text "Learned"
                    else
                        if p.learnCost <= m.freeAbilityPoints then
                            button ("Learn (" ++ String.fromInt p.learnCost ++ " AP)") (Msg.UserSelectedLearnPassive p)
                        else
                            disabledButton ("Learn (" ++ String.fromInt p.learnCost ++ " AP) (Insufficient)")
            in
            Element.column
                []
                [ Element.text p.name
                , learnElement
                ]
        
        viewLearnOneEssentia e =
            Element.column
                []
                [ Element.text e.name
                , Element.column
                    []
                    ( List.map viewLearnOneAction e.actions )
                , Element.column
                    []
                    ( List.map viewLearnOnePassive e.passives )
                ]
    in
    Element.row
        []
        ( List.map viewLearnOneEssentia learnableEssentia )

viewTownScene : Character -> Element Msg
viewTownScene m =
    Element.column
        []
        [ Element.row
            []
            [ Element.text "Home"
            , button "Go" (Msg.UserSelectedScene Scene.Home)
            ]
        , Element.row
            []
            [ Element.text "Potion Shop"
            , button "Go" (Msg.UserSelectedShop (Shop.byId "potionshop"))
            ]

        ]

button : String -> Msg -> Element Msg
button labelText msg =
    Element.Input.button
        [ Element.Background.color Palette.lightGray
        , Element.padding 10
        ]
        { onPress = Just msg
        , label = Element.text labelText
        }

coloredButton : Element.Color -> String -> Msg -> Element Msg
coloredButton color labelText msg =
    Element.Input.button
        [ Element.Background.color color
        , Element.padding 10
        ]
        { onPress = Just msg
        , label = Element.text labelText
        }

disabledButton : String -> Element Msg
disabledButton labelText =
    Element.Input.button
        [ Element.padding 10
        ]
        { onPress = Nothing
        , label = Element.text labelText
        }