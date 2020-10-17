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
import CharacterCreationSettings exposing (CharacterCreationSettings)
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

-- MODEL

type alias Model =
    { phase : Phase
    }

type Phase
    = CharacterCreationPhase CharacterCreationModel
    | ScenePhase Scene SceneModel

type alias CharacterCreationModel =
    { settings : CharacterCreationSettings
    }


type alias SceneModel =
    { name : String
    , avatar : Avatar
    , gold : Int
    , inventory : Inventory
    , level : Int
    , experience : Int
    , freeAbilityPoints : Int
    , totalAbilityPoints : Int
    , satiety : Int
    , maxSatiety : Int
    , hitPoints : Int
    , maxHitPoints : Int
    , magicPoints : Int
    , maxMagicPoints : Int
    , attack : Int
    , agility : Int
    , actions : List Action
    }

type alias Battle =
    { monster : Monster
    , player : SceneModel
    }

type alias Delve =
    { dungeon : Dungeon
    , floor : Int
    }

type DelvePhase
    = ExplorationPhase (List DungeonPath.Path)
    | ActionPhase DungeonScene.Scene

applyEffectToSceneModel : Effect -> SceneModel -> SceneModel
applyEffectToSceneModel effect m =
    case effect of
        Effect.ChangeLevel d ->
            { m | level = max 1 (m.level + d) }
        
        Effect.ChangeExperience d ->
            { m | experience = max 0 (m.experience + d) }
        
        Effect.ChangeSatiety d ->
            { m | satiety = Util.boundedBy 0 m.maxSatiety (m.satiety + d) }
        
        Effect.ChangeMaxSatiety d ->
            let
                newMaxSatiety =
                    max 1 (m.maxSatiety + d)
            in
            { m
                | maxSatiety = newMaxSatiety
                , satiety = Util.boundedBy 0 newMaxSatiety m.satiety
            }
        
        Effect.ChangeHitPoints d ->
            { m | hitPoints = Util.boundedBy 0 m.maxHitPoints (m.hitPoints + d) }
        
        Effect.ChangeMaxHitPoints d ->
            let
                newMaxHitPoints =
                    max 1 (m.maxHitPoints + d)
            in
            { m
                | maxHitPoints = newMaxHitPoints
                , hitPoints = Util.boundedBy 0 newMaxHitPoints m.hitPoints
            }
        
        Effect.ChangeMagicPoints d ->
            { m | magicPoints = Util.boundedBy 0 m.maxMagicPoints (m.magicPoints + d) }

        Effect.ChangeMaxMagicPoints d ->
            let
                newMaxMagicPoints =
                    max 1 (m.maxMagicPoints + d)
            in
            { m
                | maxMagicPoints = newMaxMagicPoints
                , magicPoints = Util.boundedBy 0 newMaxMagicPoints m.magicPoints
            }
        
        Effect.ChangeAttack d ->
            { m | attack = max 0 (m.attack + d) }
        
        _ ->
            m

applyEffectsToSceneModel : List Effect -> SceneModel -> SceneModel
applyEffectsToSceneModel effects m =
    List.foldl applyEffectToSceneModel m effects

characterCreationSettingsToSceneModel : CharacterCreationSettings -> Result (List CharacterCreationError.Error) SceneModel
characterCreationSettingsToSceneModel settings =
    FormResult.toValidation settings.name
        |> Result.andThen (\name -> FormResult.toValidation settings.hairStyle
        |> Result.andThen (\hairStyle -> FormResult.toValidation settings.hairColor
        |> Result.andThen (\hairColor -> FormResult.toValidation settings.eyeColor
        |> Result.andThen (\eyeColor -> FormResult.toValidation settings.complexion
        |> Result.andThen (\complexion -> FormResult.toValidation settings.height
        |> Result.andThen (\height -> FormResult.toValidation settings.build
        |> Result.andThen (\build -> Ok <|
            let
                avatar =
                    { hairStyle = hairStyle
                    , hairColor = hairColor
                    , eyeColor = eyeColor
                    , complexion = complexion
                    , height = height
                    , build = build
                    }
            in
            { name = name
            , avatar = avatar
            , gold = 0
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
            }
        )))))))

type Scene
    = PlayerScene
    | LearnSelectScene
    | HomeScene
    | ShopSelectScene
    | ShopScene Shop
    | ExploreScene
    | ExploreDungeonScene DelvePhase Delve
    | ExploreDoneScene Delve
    | BattleScene
    | BattleMonsterLoadingIntentScene Monster
    | BattleMonsterScene Monster Action
    | VictoryScene Monster Reward
    | GameOverScene

-- MSG

type Msg
    = NoOp
    | UserSelectedPlayerScene
    | UserSelectedLearnSelectScene
    | UserSelectedLearnSkill Action
    | UserSelectedHomeScene
    | UserSelectedHomeRest
    | UserSelectedShopSelectScene
    | UserSelectedShop Shop
    | UserSelectedBuy Item
    | UserSelectedUseItem Item
    | UserSelectedExploreScene
    | UserSelectedExploreDungeonScene Dungeon
    | SystemGotDungeonInitialization Dungeon (List DungeonPath.Path)
    | UserSelectedDungeonPath DungeonPath.Path
    | SystemGotDungeonScene DungeonScene.Scene
    | UserSelectedContinueDungeon
    | SystemGotDungeonContinuation (List DungeonPath.Path)
    | SystemGotMonster Monster
    | SystemGotMonsterIntent Action
    | SystemGotObject Object
    | UserSelectedBattleScene
    | UserSelectedBattleMonsterScene Monster
    | UserSelectedBattleAction Action
    | UserSelectedCharacterCreationSettingSelection CharacterCreationSettingSelection
    | UserSelectedCharacterCreationConfirmation
    | DevSelectedCharacterCreationConfirmation

type CharacterCreationSettingSelection
    = NameSelection String
    | HairStyleSelection HairStyle
    | HairColorSelection HairColor
    | EyeColorSelection EyeColor
    | ComplexionSelection Complexion
    | HeightSelection Height
    | BuildSelection Build

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
            { phase = CharacterCreationPhase initCharacterCreationModel
            }
    in
    ( initModel, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    case model.phase of
        CharacterCreationPhase characterCreationModel ->
            viewCharacterCreationPhase characterCreationModel
        
        ScenePhase scene sceneModel ->
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
              , Html.input [ Html.Events.onInput (UserSelectedCharacterCreationSettingSelection << NameSelection) ] []
              , settingToInfo (\a -> a) model.settings.name 
              )
            , ( "Hair Style"
              , radioButtons HairStyle.toString (UserSelectedCharacterCreationSettingSelection << HairStyleSelection) HairStyle.all model.settings.hairStyle
              , settingToInfo HairStyle.toString model.settings.hairStyle
              )
            , ( "Hair Color"
              , radioButtons HairColor.toString (UserSelectedCharacterCreationSettingSelection << HairColorSelection) HairColor.all model.settings.hairColor
              , settingToInfo HairColor.toString model.settings.hairColor
              )
            , ( "Eye Color"
              , radioButtons EyeColor.toString (UserSelectedCharacterCreationSettingSelection << EyeColorSelection) EyeColor.all model.settings.eyeColor
              , settingToInfo EyeColor.toString model.settings.eyeColor
              )
            , ( "Complexion"
              , radioButtons Complexion.toString (UserSelectedCharacterCreationSettingSelection << ComplexionSelection) Complexion.all model.settings.complexion
              , settingToInfo Complexion.toString model.settings.complexion
              )
            , ( "Height"
              , radioButtons Height.toString (UserSelectedCharacterCreationSettingSelection << HeightSelection) Height.all model.settings.height
              , settingToInfo Height.toString model.settings.height
              )
            , ( "Build"
              , radioButtons Build.toString (UserSelectedCharacterCreationSettingSelection << BuildSelection) Build.all model.settings.build
              , settingToInfo Build.toString model.settings.build
              )
            ]
        , Html.button [ Html.Events.onClick UserSelectedCharacterCreationConfirmation ] [ Html.text "Create" ]
        , Html.button [ Html.Events.onClick DevSelectedCharacterCreationConfirmation ] [ Html.text "Dev Create" ]
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
            BattleMonsterScene _ _ ->
                Html.div
                    []
                    []
            
            ExploreDungeonScene _ _ ->
                Html.div
                    []
                    []
            
            _ ->
                 buttonList
                    [ ( "Player", UserSelectedPlayerScene )
                    , ( "Learn", UserSelectedLearnSelectScene )
                    , ( "Home", UserSelectedHomeScene )
                    , ( "Shop", UserSelectedShopSelectScene )
                    , ( "Town", NoOp )
                    , ( "Explore", UserSelectedExploreScene )
                    , ( "Battle", UserSelectedBattleScene )
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
                    [ Html.Events.onClick <| UserSelectedUseItem item ]
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
        PlayerScene ->
            Html.ul
                []
                [ textList
                    [ "ATK: " ++ String.fromInt sceneModel.attack
                    , "AGI: " ++ String.fromInt sceneModel.agility
                    ]
                ]
        
        LearnSelectScene ->
            learnTable Action.learnable sceneModel.actions
        
        HomeScene ->
            buttonList
                [ ( "Rest", UserSelectedHomeRest )
                ]
        
        ShopSelectScene ->
            shopTable
                [ Shop.byId "potionshop"
                ]
        
        ShopScene shop ->
            viewShopScene sceneModel shop
        
        ExploreScene ->
            dungeonTable
                [ Dungeon.byId "beginnerscave"
                ]
        
        ExploreDungeonScene delvePhase delve ->
            viewExploreDungeonScene sceneModel delvePhase delve

        ExploreDoneScene delve ->
            textList
                [ "You beat " ++ delve.dungeon.name
                ]
        
        BattleScene ->
            monsterTable
                [ Monster.byId "dummy"
                , Monster.byId "gremlin"
                ]
        
        BattleMonsterLoadingIntentScene monster ->
            Html.text <| monster.name ++ " is thinking..."
        
        BattleMonsterScene monster intent ->
            viewBattleMonsterScene sceneModel monster intent
        
        VictoryScene monster reward ->
            textList
                [ "You defeated " ++ monster.name ++ "!"
                , "Reward:"
                , "EXP: " ++ String.fromInt reward.experience
                ]
        
        GameOverScene ->
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
                    [ Html.Events.onClick <| UserSelectedLearnSkill learn ]
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
            ExplorationPhase paths ->
                pathTable paths
            
            ActionPhase scene ->
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
                    
                    _ ->
                        Html.ul
                            []
                            [ Html.text <| DungeonScene.toString scene
                            , Html.button
                                [ Html.Events.onClick <| UserSelectedContinueDungeon ]
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
                    [ Html.Events.onClick <| UserSelectedDungeonPath path ]
                    [ Html.text "Go" ]
                ]
    in
    Html.ul
        []
        ( List.map pathFn paths )

shopTable : List Shop -> Html Msg
shopTable shops =
    let
        shopFn shop =
            Html.li
                []
                [ Html.text shop.name
                , Html.button
                    [ Html.Events.onClick <| UserSelectedShop shop ]
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
    textList (List.map explainOneScene (d.head :: d.tail))

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
                    [ Html.Events.onClick <| UserSelectedBattleAction action ]
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
                    [ Html.Events.onClick <| UserSelectedBuy b ]
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
                    [ Html.Events.onClick <| UserSelectedExploreDungeonScene dungeon ]
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
                    [ Html.Events.onClick <| UserSelectedBattleMonsterScene monster ]
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
        ( UserSelectedPlayerScene, ScenePhase _ sceneModel ) ->
            ( { model | phase = ScenePhase PlayerScene sceneModel }, Cmd.none )
        
        ( UserSelectedLearnSelectScene, ScenePhase _ sceneModel ) ->
            ( { model | phase = ScenePhase LearnSelectScene sceneModel }, Cmd.none )
        
        ( UserSelectedLearnSkill action, ScenePhase scene sceneModel ) ->
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
            ( { model | phase = ScenePhase scene newSceneModel }, Cmd.none )
        
        ( UserSelectedHomeScene, ScenePhase _ sceneModel ) ->
            ( { model | phase = ScenePhase HomeScene sceneModel }, Cmd.none )
        
        ( UserSelectedHomeRest, ScenePhase scene sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel 
                        | hitPoints = sceneModel.maxHitPoints
                        , magicPoints = sceneModel.maxMagicPoints
                    }
            in
            ( { model | phase = ScenePhase scene newSceneModel }, Cmd.none )
        
        ( UserSelectedShopSelectScene, ScenePhase _ sceneModel ) ->
            ( { model | phase = ScenePhase ShopSelectScene sceneModel }, Cmd.none )
        
        ( UserSelectedShop shop, ScenePhase ShopSelectScene sceneModel ) ->
            ( { model | phase = ScenePhase (ShopScene shop) sceneModel }, Cmd.none )
        
        ( UserSelectedBuy item, ScenePhase scene sceneModel ) ->
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
            ( { model | phase = ScenePhase scene newSceneModel }, Cmd.none )
        
        ( UserSelectedUseItem item, ScenePhase scene sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel
                        | inventory =
                            sceneModel.inventory
                                |> Inventory.modify item -1
                    }
                        |> applyEffectsToSceneModel item.effects
            in
            ( { model | phase = ScenePhase scene newSceneModel }, Cmd.none )
        
        ( UserSelectedExploreScene, ScenePhase _ sceneModel ) ->
            ( { model | phase = ScenePhase ExploreScene sceneModel }, Cmd.none )
        
        ( UserSelectedExploreDungeonScene dungeon, ScenePhase ExploreScene _ ) ->
            let
                pathListGenerator =
                    Random.list 3 DungeonPath.generator
                
                cmd =
                    Random.generate (SystemGotDungeonInitialization dungeon) pathListGenerator
            in
            ( model, cmd )
        
        ( SystemGotDungeonInitialization dungeon paths, ScenePhase ExploreScene sceneModel ) ->
            let
                delve =
                    { dungeon = dungeon
                    , floor = 1
                    }
            in
            ( { model | phase = ScenePhase (ExploreDungeonScene (ExplorationPhase paths) delve) sceneModel }, Cmd.none )
        
        ( UserSelectedDungeonPath path, ScenePhase (ExploreDungeonScene _ _) _) ->
            let
                cmd =
                    Random.generate SystemGotDungeonScene (Distribution.random path.sceneDistribution)
            in
            ( model, cmd )
        
        ( SystemGotDungeonScene scene, ScenePhase (ExploreDungeonScene delvePhase delve) sceneModel ) ->
            let
                cmd =
                    case scene of
                        DungeonScene.Battle ->
                            Random.generate SystemGotMonster Monster.generator
                        
                        DungeonScene.Treasure ->
                            Random.generate SystemGotObject Object.generator
                        
                        _ ->
                            Cmd.none
                
            in
            ( { model | phase = ScenePhase (ExploreDungeonScene (ActionPhase scene) delve) sceneModel }, cmd )
        
        ( SystemGotMonster monster, ScenePhase (ExploreDungeonScene delvePhase delve) sceneModel ) ->
            let
                cmd =
                    Random.generate SystemGotMonsterIntent (Monster.chooseAction monster)
            in
            ( { model | phase = ScenePhase (ExploreDungeonScene (ActionPhase (DungeonScene.BattleMonsterLoadingIntent monster)) delve) sceneModel }, cmd )
        
        ( SystemGotMonsterIntent intent, ScenePhase (ExploreDungeonScene (ActionPhase (DungeonScene.BattleMonsterLoadingIntent monster)) delve) sceneModel ) ->
            ( { model | phase = ScenePhase (ExploreDungeonScene (ActionPhase (DungeonScene.BattleMonster monster intent)) delve) sceneModel }, Cmd.none )

        ( SystemGotObject (Object.Item i), ScenePhase (ExploreDungeonScene _ delve) sceneModel ) ->
            let
                newDungeonScene =
                    ActionPhase <| DungeonScene.ReceiveTreasure (Object.Item i)
                
                newSceneModel =
                    { sceneModel
                        | inventory =
                            sceneModel.inventory
                                |> Inventory.modify i 1
                    }
                
                newModel =
                    { model
                        | phase = ScenePhase (ExploreDungeonScene newDungeonScene delve) newSceneModel
                    }
            in
            ( newModel, Cmd.none )
        
        ( UserSelectedContinueDungeon, ScenePhase (ExploreDungeonScene _ _) _) ->
            let
                pathListGenerator =
                    Random.list 3 DungeonPath.generator

                cmd =
                    Random.generate SystemGotDungeonContinuation pathListGenerator
            in
            ( model, cmd )
        
        ( SystemGotDungeonContinuation paths, ScenePhase (ExploreDungeonScene _ delve) sceneModel ) ->
            let
                newScene =
                    if delve.floor >= delve.dungeon.depth then
                        ExploreDoneScene delve
                    else
                        let
                            newDelve =
                                { delve
                                    | floor =
                                        delve.floor + 1
                                            |> Util.boundedBy 1 delve.dungeon.depth
                                }
                        in
                        ExploreDungeonScene (ExplorationPhase paths) newDelve
            in 
            ( { model | phase = ScenePhase newScene sceneModel }, Cmd.none )
        
        ( UserSelectedBattleScene, ScenePhase _ sceneModel ) ->
            ( { model | phase = ScenePhase BattleScene sceneModel }, Cmd.none )
        
        ( UserSelectedBattleMonsterScene monster, ScenePhase _ sceneModel ) ->
            let
                cmd =
                    Random.generate SystemGotMonsterIntent (Monster.chooseAction monster)
            in
            ( { model | phase = ScenePhase (BattleMonsterLoadingIntentScene monster) sceneModel }, cmd )
        
        ( SystemGotMonsterIntent intent, ScenePhase (BattleMonsterLoadingIntentScene monster) sceneModel ) ->
            ( { model | phase = ScenePhase (BattleMonsterScene monster intent) sceneModel }, Cmd.none )
        
        ( UserSelectedBattleAction action, ScenePhase (BattleMonsterScene monster monsterAction) sceneModel ) ->
            updateBattleAction model monster action monsterAction sceneModel
        
        ( UserSelectedBattleAction action, ScenePhase (ExploreDungeonScene (ActionPhase (DungeonScene.BattleMonster monster monsterAction)) delve) sceneModel ) ->
            updateDungeonBattleAction model monster action monsterAction delve sceneModel
        
        ( UserSelectedCharacterCreationSettingSelection selection, CharacterCreationPhase characterCreationModel ) ->
            updateCharacterCreationSettingSelection model characterCreationModel selection
        
        ( UserSelectedCharacterCreationConfirmation, CharacterCreationPhase characterCreationModel ) ->
            updateCharacterCreationConfirmation model characterCreationModel
        
        ( DevSelectedCharacterCreationConfirmation, CharacterCreationPhase _ ) ->
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
                    }
                
                newModel =
                    { model | phase = ScenePhase PlayerScene sceneModel } 
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
                NameSelection name ->
                    { settings | name = FormResult.FROk name }
                
                HairStyleSelection hairStyle ->
                    { settings | hairStyle = FormResult.FROk hairStyle }
                
                HairColorSelection hairColor ->
                    { settings | hairColor = FormResult.FROk hairColor }
                
                EyeColorSelection eyeColor ->
                    { settings | eyeColor = FormResult.FROk eyeColor }
                
                ComplexionSelection complexion ->
                    { settings | complexion = FormResult.FROk complexion }
                
                HeightSelection height ->
                    { settings | height = FormResult.FROk height }
                
                BuildSelection build ->
                    { settings | build = FormResult.FROk build }
    
        newCharacterCreationModel =
            { settings = newSettings
            }
        
        newModel =
            { model | phase = CharacterCreationPhase newCharacterCreationModel }
    in
    ( newModel, Cmd.none )

updateCharacterCreationConfirmation : Model -> CharacterCreationModel -> ( Model, Cmd Msg )
updateCharacterCreationConfirmation model characterCreationModel =
    let
        newSettings =
            CharacterCreationSettings.check characterCreationModel.settings
        
        sceneModelResult =
            characterCreationSettingsToSceneModel newSettings
        
        newModel =
            case sceneModelResult of
                Ok sceneModel ->
                    { model | phase = ScenePhase PlayerScene sceneModel }
                
                Err _ ->
                    { model | phase = CharacterCreationPhase { settings = newSettings }}

    in
    ( newModel, Cmd.none )

updateBattleAction : Model -> Monster -> Action -> Action -> SceneModel -> ( Model, Cmd Msg )
updateBattleAction model monster action monsterAction sceneModel =
    let
        ( newMonster, newSceneModel ) =
            runOneBattleRound monsterAction action monster sceneModel
        
        ( newScene, newSceneModel2 ) =
            if newSceneModel.hitPoints <= 0 then
                ( GameOverScene, newSceneModel )
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
                ( VictoryScene newMonster reward, newSceneModel3 )

            else
                ( BattleMonsterLoadingIntentScene newMonster, newSceneModel )
    in
    ( { model | phase = ScenePhase newScene newSceneModel2 }, Random.generate SystemGotMonsterIntent (Monster.chooseAction monster) )

updateDungeonBattleAction : Model -> Monster -> Action -> Action -> Delve -> SceneModel -> ( Model, Cmd Msg )
updateDungeonBattleAction model monster action monsterAction delve sceneModel =
    let
        ( newMonster, newSceneModel ) =
            runOneBattleRound monsterAction action monster sceneModel
        
        ( newScene, newSceneModel2 ) =
            if newSceneModel.hitPoints <= 0 then
                ( GameOverScene, newSceneModel )
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
                ( ExploreDungeonScene (ActionPhase (DungeonScene.Victory newMonster reward)) delve, newSceneModel3 )

            else
                ( ExploreDungeonScene (ActionPhase (DungeonScene.BattleMonsterLoadingIntent newMonster)) delve, newSceneModel )
    in
    ( { model | phase = ScenePhase newScene newSceneModel2 }, Random.generate SystemGotMonsterIntent (Monster.chooseAction monster) )


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

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none