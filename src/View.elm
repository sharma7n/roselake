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
            , ( "Starting Weapon"
              , radioButtons .name (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.StartingWeaponSelection) Weapon.listStarting model.settings.startingWeapon
              , settingToInfo .name model.settings.startingWeapon
              )
            , ( "Starting Essentia"
              , radioButtons .name (Msg.UserSelectedCharacterCreationSettingSelection << CharacterCreationSettingSelection.StartingEssentiaSelection) Essentia.listStarting model.settings.startingEssentia
              , settingToInfo .name model.settings.startingEssentia
              )
            ]
        , Html.text <| "Attribute Points: " ++ String.fromInt model.attributePoints
        , Html.button [ Html.Events.onClick Msg.UserSelectedRandomCharacterCreation ] [ Html.text "Randomize" ]
        , Html.button [ Html.Events.onClick Msg.UserSelectedCharacterCreationConfirmation ] [ Html.text "Create" ]
        , Html.button [ Html.Events.onClick Msg.DevSelectedCharacterCreationConfirmation ] [ Html.text "Dev Create" ]
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


viewScenePhase : Scene -> SceneModel -> Html Msg
viewScenePhase scene sceneModel =
    Html.div
        []
        [ Html.ul
            []
            [ textListItem <| sceneModel.name
            , textListItem <| Avatar.description sceneModel.avatar
            , textListItem <| "G: " ++ String.fromInt sceneModel.gold
            , textListItem <| "LV: " ++ String.fromInt sceneModel.level
            , textListItem <| "EXP: " ++ String.fromInt sceneModel.experience ++ " / " ++ String.fromInt (levelUpExperience sceneModel.level)
            , textListItem <| "AP: " ++ String.fromInt sceneModel.freeAbilityPoints ++ " / " ++ String.fromInt sceneModel.totalAbilityPoints
            , statusSetListItem sceneModel.statusSet
            , textListItem <| "Satiety: " ++ String.fromInt sceneModel.satiety ++ " / " ++ String.fromInt sceneModel.maxSatiety
            , textListItem <| "HP: " ++ String.fromInt sceneModel.hitPoints ++ " / " ++ String.fromInt (Battler.totalMaxHitPoints sceneModel)
            , textListItem <| "MP: " ++ String.fromInt sceneModel.magicPoints ++ " / " ++ String.fromInt sceneModel.maxMagicPoints
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
                 buttonBar
                    [ ( "Player", Msg.UserSelectedScene Scene.PlayerScene )
                    , ( "Essentia", Msg.UserSelectedScene Scene.EssentiaScene )
                    , ( "Learn", Msg.UserSelectedScene Scene.LearnSelectScene )
                    , ( "Equip", Msg.UserSelectedScene Scene.EquipScene )
                    , ( "Home", Msg.UserSelectedScene Scene.HomeScene )
                    , ( "Shop", Msg.UserSelectedScene Scene.ShopSelectScene )
                    , ( "Town", Msg.NoOp )
                    , ( "Explore", Msg.UserSelectedScene Scene.ExploreScene )
                    , ( "Battle", Msg.UserSelectedScene Scene.BattleScene )
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
    

viewSceneModel : Scene -> SceneModel -> Html Msg
viewSceneModel scene sceneModel =
    case scene of
        Scene.PlayerScene ->
            Html.ul
                []
                [ Html.ol
                    []
                    [ Html.text "Attributes"
                    , textList
                        [ "STR: " ++ String.fromInt sceneModel.attack
                        , "VIT: " ++ String.fromInt sceneModel.vitality
                        , "AGI: " ++ String.fromInt sceneModel.agility
                        , "INT: " ++ String.fromInt sceneModel.magic
                        , "CHA: " ++ String.fromInt sceneModel.charisma
                        ]
                    , Html.text "Equipment"
                    , textList
                        [ "Weapon: " ++ Maybe.withDefault " - " (Maybe.map .name sceneModel.equippedWeapon)
                        , "Armor: " ++ Maybe.withDefault " - " (Maybe.map .name sceneModel.equippedArmor)
                        ]
                    , Html.text "Passives"
                    , textList
                        ( sceneModel.learnedPassives
                            |> Set.toList
                            |> List.map Passive.byId
                            |> List.map .name
                        )
                    , Html.text "Actions"
                    , textList
                        ( sceneModel.learned
                            |> Set.toList
                            |> List.map Action.byId
                            |> List.map .name
                        )
                    ]
                ]
        
        Scene.EssentiaScene ->
            viewEssentiaScene sceneModel.essentia sceneModel.essentiaContainer
        
        Scene.LearnSelectScene ->
            viewLearnScene sceneModel
        
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
                
                equippedArmorElement =
                    case sceneModel.equippedArmor of
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
                    (List.map equipableFn (Inventory.listWeapons sceneModel.inventory))
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
                    (List.map equipableFn (Inventory.listArmors sceneModel.inventory))
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
                [ MonsterTemplate.byId "dummy"
                , MonsterTemplate.byId "gremlin"
                , MonsterTemplate.byId "wyvern"
                ]
        
        Scene.BattleMonsterLoadingIntentScene battle ->
            Html.text <| battle.monster.name ++ " is thinking..."
        
        Scene.BattleMonsterScene battle intent ->
            viewBattleMonsterScene sceneModel battle intent
        
        Scene.VictoryLoadingScene _ ->
            Html.text "Loading..."
        
        Scene.VictoryScene battle reward ->
            textList
                [ "You defeated " ++ battle.monster.name ++ "!"
                , "Reward:"
                , "EXP: " ++ String.fromInt reward.experience
                ]
        
        Scene.GameOverScene ->
            textList
                [ "Defeated..."
                ]

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
                    DungeonScene.BattleMonster battle intent ->
                        viewBattleMonsterScene sceneModel battle intent
                    
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

viewBattleMonsterScene : SceneModel -> Battle -> Action -> Html Msg
viewBattleMonsterScene sceneModel battle intent =
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
            , textListItem <| sceneModel.name
            , statusSetListItem <| sceneModel.statusSet
            , textListItem <| "HP: " ++ String.fromInt sceneModel.hitPoints ++ " / " ++ String.fromInt (Battler.totalMaxHitPoints sceneModel)
            , textListItem <| "AP: " ++ String.fromInt sceneModel.actionPoints ++ " / " ++ String.fromInt sceneModel.maxActionPoints
            , textListItem <| "Block: " ++ String.fromInt sceneModel.block
            ]
        , actionTable sceneModel.actionPoints sceneModel.actionStates
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

viewLearnScene : SceneModel -> Html Msg
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