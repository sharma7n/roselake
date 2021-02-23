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
import Map exposing (Map)
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
import Html
import Button
import Ui
import Ui
import Ui

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
            [ Button.button "Randomize" Msg.UserSelectedRandomCharacterCreation
            , Button.button "Create" Msg.UserSelectedCharacterCreationConfirmation
            , Button.button "Dev Create" Msg.DevSelectedCharacterCreationConfirmation
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
            ( Button.coloredButton Palette.lightRed "-" (Msg.UserSelectedModifyCharacterCreationAttribute attr (-1)) )
        , Element.el
            [ Element.centerX
            ]
            ( Element.text <| Attribute.toShortString attr ++ ": " ++ String.fromInt value )
        , Element.el
            [ Element.alignRight
            ]
            ( Button.coloredButton Palette.lightGreen "+" (Msg.UserSelectedModifyCharacterCreationAttribute attr 1) )
        ]

statusSetListItem : StatusSet -> Element Msg
statusSetListItem statusSet =
    Element.column
        []
        [ viewStatusSet statusSet
        ]


viewScenePhase : Scene -> SceneState -> Character -> Element Msg
viewScenePhase scene sceneState character =
    Element.column
        [ Element.spacing 10
        ]
        [ viewName character.name
        , Element.row
            [ Element.spacing 10
            ]
            [ viewAvatar character.avatar
            , viewQuickStats character
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
                    , ( "Inventory", Msg.UserSelectedScene Scene.Inventory )
                    , ( "Town", Msg.UserSelectedScene Scene.Town )
                    , ( "Explore", Msg.UserSelectedScene Scene.DungeonSelect )
                    ]
        , viewCharacter scene sceneState character
        ]

viewName : String -> Element Msg
viewName name =
    Element.el
        [ Element.Font.heavy
        ]
        ( Element.text name )

viewQuickStats : Character -> Element Msg
viewQuickStats character =
    Element.column
        [ Element.spacing 5
        , Element.padding 10
        , Element.Background.color Palette.lightBlue
        ]
        [ textPair "LV" (String.fromInt character.level)
        , textPair "EXP" (ratio character.experience (levelUpExperience character.level))
        , textPair "AbP" (ratio character.freeAbilityPoints character.totalAbilityPoints)
        , textPair "HP" (ratio character.hitPoints (Battler.totalMaxHitPoints character))
        , textPair "MP" (ratio character.magicPoints character.maxMagicPoints)
        , textPair "G" (String.fromInt character.gold)
        , textPair "AcP" (ratio character.actionPoints character.maxActionPoints)
        , textPair "Block" (String.fromInt character.block)
        ]

textPair : String -> String -> Element Msg
textPair key value =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 25
        ]
        [ Element.el
            [ Element.alignLeft
            ]
            ( Element.text key )
        , Element.el
            [ Element.alignRight
            ]
            ( Element.text value )
        ]

ratio : Int -> Int -> String
ratio top bot =
    String.fromInt top ++ " / " ++ String.fromInt bot

viewAvatar : Avatar -> Element Msg
viewAvatar avatar =
    Element.column
        [ Element.spacing 5
        , Element.padding 10
        , Element.alignTop
        , Element.Background.color Palette.lightBlue
        ]
        [ Element.text <| "Build: " ++ Height.toString avatar.height ++ " & " ++ Build.toString avatar.build
        , Element.text <| "Complexion: " ++ Complexion.toString avatar.complexion
        , Element.text <| "Hair: " ++ HairStyle.toString avatar.hairStyle ++ " & " ++ HairColor.toString avatar.hairColor
        , Element.text <| "Eye Color: " ++ EyeColor.toString avatar.eyeColor
        ]

viewStatusSet : StatusSet -> Element Msg
viewStatusSet s =
    viewStatusData (StatusSet.toList s)

viewStatusData : List StatusSet.Data -> Element Msg
viewStatusData data =
    let
        viewOneStatusData datum =
            Element.text <| Status.toString datum.status ++ ": " ++ String.fromInt datum.stacks
    in
    Ui.column
        ( List.map viewOneStatusData data )

viewInventory : Inventory -> Element Msg
viewInventory i =
    i
        |> Inventory.listItems
        |> List.filter (\(_, q) -> q > 0)
        |> table (\( item, qty ) ->
            Ui.row
                [ Element.text <| item.name ++ ": "
                , Element.text <| String.fromInt qty ++ " "
                , Button.button "Use" (Msg.UserSelectedUseItem item)
                ]
        )

textList : List String -> Element Msg
textList items =
    let
        itemFn item =
            Element.text item
    in
    Ui.column
        ( List.map itemFn items )

buttonList : List ( String, Msg ) -> Element Msg
buttonList items =
    let
        itemFn ( label, msg ) =
            Button.button label msg
    in
    Element.column
        []
        ( List.map itemFn items )

buttonBar : List ( String, Msg ) -> Element Msg
buttonBar items =
    let
        itemFn ( label, msg ) =
            Button.button label msg
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
            viewPlayer character
        
        Scene.Essentia ->
            viewEssentiaScene character.essentia character.essentiaContainer
        
        Scene.LearnSelect ->
            viewLearnScene character
        
        Scene.Equip ->
            let
                equippedWeaponElement =
                    case character.equippedWeapon of
                        Just weapon ->
                            Ui.column
                                [ Element.text <| "Weapon: " ++ weapon.name
                                , Button.button "Un-Equip" (Msg.UserSelectedUnEquipWeapon weapon)
                                ]
                        
                        Nothing ->
                            Ui.column
                                [ Element.text <| "Weapon: - "
                                ]
                
                equippedArmorElement =
                    case character.equippedArmor of
                        Just armor ->
                            Ui.column
                                [ Element.text <| "Armor: " ++ armor.name
                                , Button.button "Un-Equip" (Msg.UserSelectedUnEquipArmor armor)
                                ]
                        
                        Nothing ->
                            Element.text <| "Armor: - "
            in
            Ui.column
                [ Element.text "Equipped"
                , Ui.column
                    [ equippedWeaponElement
                    , equippedArmorElement
                    ]
                , Element.text "Equipable"
                , let

                    equipableFn (w, q) =
                        Ui.column
                            [ Element.text <| w.name ++ " (" ++ String.fromInt q ++ ")"
                            , Button.button "Equip" (Msg.UserSelectedEquipWeapon w)
                            ]
                  in
                  Ui.column
                    (List.map equipableFn (Inventory.listWeapons character.inventory))
                , let

                    equipableFn (w, q) =
                        Ui.column
                            [ Element.text <| w.name ++ " (" ++ String.fromInt q ++ ")"
                            , Button.button "Equip" (Msg.UserSelectedEquipArmor w)
                            ]
                  in
                  Ui.column
                    (List.map equipableFn (Inventory.listArmors character.inventory))
                ]
        
        Scene.Home ->
            viewHome
        
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
            viewDungeonSelect character.maps
        
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
                            Ui.column
                                [ victoryMessage
                                ]
                        
                        SceneState.Delving _ _ ->
                            Ui.column
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
        
        Scene.Inventory ->
            viewInventory character.inventory

continueButton : Element Msg
continueButton =
    Button.button "Continue" Msg.UserSelectedContinueDungeon

exitButtonDungeon : Element Msg
exitButtonDungeon =
    Button.button "Exit Dungeon" Msg.UserSelectedExitDungeon

viewRequirements : List Requirement -> Element Msg
viewRequirements l =
    let
        viewOneRequirement r =
            Element.text <| "Requires: " ++ Requirement.toString r
    in
    Ui.column
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
              Ui.column
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
    Ui.column
        display

shopTable : List Shop -> Element Msg
shopTable shops =
    shops
        |> table (\shop ->
            Ui.column
                [ Element.text shop.name
                , Button.button "Go" (Msg.UserSelectedShop shop)
                ]
        )
    
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
    Ui.column
        [ textList
            [ "Round: " ++ String.fromInt battle.round
            ]
        , Ui.column
            [ Element.text <| "Enemy"
            , Element.text <| monster.name
            , viewStatusSet <| monster.statusSet
            , Element.text <| "HP: " ++ String.fromInt monster.hitPoints ++ " / " ++ String.fromInt (Battler.totalMaxHitPoints monster)
            , Element.text <| "Intent: " ++ intent.name
            , Element.text <| "Block: " ++ String.fromInt monster.block
            ]
        , viewActions character.actionPoints character.actionStates
        , Button.button "End Turn" (Msg.UserSelectedEndBattleTurn)
        ]

levelUpExperience : Int -> Int
levelUpExperience n =
    10 * n * n

monsterTable : List MonsterTemplate -> Element Msg
monsterTable monsterTemplates =
    monsterTemplates
        |> table (\monsterTemplate ->
            Ui.column
                [ Element.text <| monsterTemplate.name
                , Element.text <| " | HP: " ++ String.fromInt monsterTemplate.hitPoints
                , Element.text <| " | EXP: " ++ String.fromInt monsterTemplate.experience ++ " "
                , Button.button "Fight" (Msg.UserSelectedMonsterTemplate monsterTemplate)
                ]
        )

viewEssentiaScene : List Essentia -> EssentiaContainer -> Element Msg
viewEssentiaScene e c =
    let
        containerSlotFn ( slotIndex, slot ) =
            case slot of
                Just essentia ->
                    Ui.column
                        [ Element.text <| "Slot " ++ EssentiaContainer.indexToString slotIndex ++ ": " ++ essentia.name
                        , Button.button "Un-Equip" (Msg.UserSelectedUnEquipEssentia slotIndex essentia)
                        ]
                
                Nothing ->
                    Ui.column
                        [ Element.text <| "Slot " ++ EssentiaContainer.indexToString slotIndex ++ ": -"
                        ]

        equipEssentiaButtonFn esn listIdx slotIndex =
            Button.button ("Equip in Slot " ++ EssentiaContainer.indexToString slotIndex) (Msg.UserSelectedEquipEssentia slotIndex listIdx esn)

        essentiaFn listIdx esn =
           Ui.column
                ( Element.text esn.name
                :: ( List.map (equipEssentiaButtonFn esn listIdx) EssentiaContainer.listIndices )
                )
    in
    Ui.column
        [ Element.text "Equipped"
        , Ui.column
            ( List.map containerSlotFn (EssentiaContainer.toList c))
        , Element.text "Equipable"
        , Ui.column
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
                            Button.button ("Learn (" ++ String.fromInt a.learnCost ++ " AP)") (Msg.UserSelectedLearnSkill a)
                        else
                            Button.disabledButton ("Learn (" ++ String.fromInt a.learnCost ++ " AP) (Insufficient)")
            in
            Ui.column
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
                            Button.button ("Learn (" ++ String.fromInt p.learnCost ++ " AP)") (Msg.UserSelectedLearnPassive p)
                        else
                            Button.disabledButton ("Learn (" ++ String.fromInt p.learnCost ++ " AP) (Insufficient)")
            in
            Ui.column
                [ Element.text p.name
                , learnElement
                ]
        
        viewLearnOneEssentia e =
            Ui.column
                [ Element.text e.name
                , Ui.column
                    ( List.map viewLearnOneAction e.actions )
                , Ui.column
                    ( List.map viewLearnOnePassive e.passives )
                ]
    in
    Ui.row
        ( List.map viewLearnOneEssentia learnableEssentia )

viewTownScene : Character -> Element Msg
viewTownScene m =
    Ui.column
        [ Ui.row
            [ viewName "Home"
            , Button.button "Go" (Msg.UserSelectedScene Scene.Home)
            ]
        , Ui.row
            [ viewName "Potion Shop"
            , Button.button "Go" (Msg.UserSelectedShop (Shop.byId "potionshop"))
            ]
        ]

table : (a -> Element msg) -> List a -> Element msg
table toElement xs =
    Ui.column
        ( List.map toElement xs )

viewHome : Element Msg
viewHome =
    Ui.column
        [ viewName "Home"
        , Element.text """
It's your home! It's small and basic, but it's yours.
        """
        , Ui.row
            [ Ui.column
                [ viewName "Rest"
                , Element.text "Catch some z's!"
                ]
            , Ui.column
                [ viewName "Effects"
                , Element.text "Restore 100% of Max HP"
                , Element.text "Restore 100% of Max MP"
                ]
            , Button.button "Rest" Msg.UserSelectedHomeRest
            ]
        , Button.button "Back to Town" (Msg.UserSelectedScene Scene.Town)
        ]

viewShopScene : Character -> Shop -> Element Msg
viewShopScene character shop =
    let
        objectRow object =
            Ui.row
                [ Ui.column
                    [ viewName <| Object.name object
                    , Element.text <| String.fromInt (Object.cost object) ++ "G"
                    ]
                , Ui.column
                    [ viewName "Effects"
                    ]
                , Button.button "Buy" (Msg.UserSelectedBuy object)
                ]  
    in
    Ui.column
        ( [ viewName <| "Shop: " ++ shop.name
          , Element.text """
It's a shop!
        """
          ] ++
          ( List.map objectRow shop.stock ) ++
          [ Button.button "Back to Town" (Msg.UserSelectedScene Scene.Town)
          ]
        )

viewDungeonSelect : List Map -> Element Msg
viewDungeonSelect maps =
    let
        mapSelectRow map_ =
            Ui.row
                [ viewName <| Map.name map_
                , Button.button "Explore" (Msg.UserSelectedExploreDungeonScene map_)
                ]  
    in
    Ui.column
        ( [ viewName "Dungeons"
          , Element.text """
Select a dungeon!
          """
          ] ++
          ( List.map mapSelectRow maps )
        )

viewExploreDungeon : Character -> DelvePhase -> Delve -> Element Msg
viewExploreDungeon character delvePhase delve =
    Ui.column
        [ viewName <| "Exploring: " ++ Map.name delve.dungeon.map
        , viewName <| "Floor: " ++ String.fromInt delve.floor ++ " / " ++ String.fromInt delve.dungeon.depth
        , case delvePhase of
            DelvePhase.ExplorationPhase paths ->
                Ui.column
                    [ viewDungeonPaths character paths
                    , Ui.column (
                        Dungeon.describeCurrentPosition delve.dungeon
                            |> List.map Element.text
                        )
                    ]
            
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
                        Ui.column
                            [ Element.text "A trap door!"
                            , exitButtonDungeon
                            ]
                    
                    DungeonScene.Goal reward ->
                        Ui.column
                            [ Element.text "Goal!"
                            , viewReward reward
                            , exitButtonDungeon
                            ]
                    
                    DungeonScene.Treasure ->
                        Ui.column
                            [ Element.text "You find a treasure chest!"
                            , Button.button "Open" Msg.UserSelectedOpenChest
                            , continueButton
                            ]
                    
                    DungeonScene.Shopping shop ->
                        Ui.column
                            [ viewShopScene character shop
                            , continueButton
                            ]
                    
                    _ ->
                        Ui.column
                            [ Element.text <| DungeonScene.toString scene
                            , continueButton
                            ]
        ]

viewPlayer : Character -> Element Msg
viewPlayer character =
    Ui.column
        [ viewName "Attributes"
        , Ui.column
            [ Element.text <| "STR: " ++ String.fromInt character.strength
            , Element.text <| "VIT: " ++ String.fromInt character.vitality
            , Element.text <| "AGI: " ++ String.fromInt character.agility
            , Element.text <| "INT: " ++ String.fromInt character.intellect
            ]
        , viewName "Equipment"
        , Ui.column
            [ Element.text <| "Weapon: " ++ Maybe.withDefault " - " (Maybe.map .name character.equippedWeapon)
            , Element.text <| "Armor: " ++ Maybe.withDefault " - " (Maybe.map .name character.equippedArmor)
            ]
        , viewName "Passives"
        , Ui.column
            ( character.learnedPassives
                |> Set.toList
                |> List.map Passive.byId
                |> List.map .name
                |> List.map Element.text
            )
        , viewName "Actions"
        , Ui.column
            ( character.learned
                |> Set.toList
                |> List.map Action.byId
                |> List.map .name
                |> List.map Element.text
            )
        ]

viewDungeonPaths : Character -> List DungeonPath.Path -> Element Msg
viewDungeonPaths m paths =
    let
        viewDungeonPath path =
            let
                can =
                    m
                        |> Character.satisfiesRequirements path.requirements
                
                buttonEl =
                    if can then
                        Button.button "Explore" (Msg.UserSelectedDungeonPath path)
                    else
                        Ui.column
                            [ viewName "Locked"
                            , viewRequirements path.requirements
                            ]
            in
            Ui.row
                [ Ui.column
                    [ viewName path.description
                    ]
                , Ui.column
                    [ viewName "Effects"
                    , explainSceneDistribution path.sceneDistribution
                    ]
                , buttonEl
                ]
    in
    Ui.column
        ( List.map viewDungeonPath paths )

viewActions : Int -> List ActionState -> Element Msg
viewActions actionPoints actionStates =
    let
        viewOneAction actionState =
            let
                buttonEl =
                    if ActionState.canUse actionPoints actionState then
                        Button.button "Go" (Msg.UserSelectedBattleAction actionState.action)
                    else
                        Ui.column
                            [ viewName "State"
                            , Element.text <| ActionState.stateToString actionState.state
                            ]
            in
            Ui.row
                [ Ui.column
                    [ viewName actionState.action.name
                    ]
                , Ui.column
                    [ viewName "AP"
                    , Element.text <| String.fromInt actionState.action.actionPointCost
                    ]
                , buttonEl
                ]
        
    in
    column actionStates viewOneAction

column : List a -> (a -> Element msg) -> Element msg
column xs f =
    Ui.column (List.map f xs)