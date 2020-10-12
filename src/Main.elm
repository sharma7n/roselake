module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Random

import Distribution exposing (Distribution)

import FormResult exposing (FormResult)
import HairStyle exposing (HairStyle)
import HairColor exposing (HairColor)
import EyeColor exposing (EyeColor)
import Complexion exposing (Complexion)
import Height exposing (Height)
import Build exposing (Build)

import DungeonPath
import DungeonScene
import Dungeon exposing (Dungeon)
import Action exposing (Action)
import BattleEffect exposing (BattleEffect)
import Monster exposing (Monster)
import Reward exposing (Reward)

-- MODEL

type alias Model =
    { phase : Phase
    }

type Phase
    = CharacterCreationPhase CharacterCreationModel
    | ScenePhase SceneModel

type alias CharacterCreationModel =
    { settings : CharacterCreationSettings
    }

type alias CharacterCreationSettings =
    { name : FormResult CharacterCreationError String
    , hairStyle : FormResult CharacterCreationError HairStyle
    , hairColor : FormResult CharacterCreationError HairColor
    , eyeColor : FormResult CharacterCreationError EyeColor
    , complexion : FormResult CharacterCreationError Complexion
    , height : FormResult CharacterCreationError Height
    , build : FormResult CharacterCreationError Build
    }

checkCharacterCreationSettings : CharacterCreationSettings -> CharacterCreationSettings
checkCharacterCreationSettings settings =
    let
        checkName name =
            case name of
                FormResult.FROk okName ->
                    if okName == "" then 
                        FormResult.FRErr MissingName 
                    else   
                        FormResult.FROk okName
                
                _ ->
                    FormResult.FRErr MissingName
    in
    { name = checkName settings.name
    , hairStyle = FormResult.check settings.hairStyle
    , hairColor = FormResult.check settings.hairColor
    , eyeColor = FormResult.check settings.eyeColor
    , complexion = FormResult.check settings.complexion
    , height = FormResult.check settings.height
    , build = FormResult.check settings.build
    }
type alias SceneModel =
    { scene : Scene
    , name : String
    , avatar : Avatar
    , level : Int
    , experience : Int
    , satiety : Int
    , maxSatiety : Int
    , hitPoints : Int
    , maxHitPoints : Int
    , magicPoints : Int
    , maxMagicPoints : Int
    , attack : Int
    }

type alias Battle =
    { monster : Monster
    , player : SceneModel
    }

type alias Delve =
    { dungeon : Dungeon
    , floor : Int
    , phase : DelvePhase
    }

type DelvePhase
    = ExplorationPhase (List DungeonPath.Path)
    | ActionPhase DungeonScene.Scene


applyEffectToBattle : BattleEffect -> Battle -> Battle
applyEffectToBattle effect b =
    case effect of
        BattleEffect.ChangePlayerHitPoints d ->
            let
                p =
                    b.player
                
                newHitPoints =
                    boundedBy 0 p.maxHitPoints (p.hitPoints + d)
                
                newPlayer =
                    { p | hitPoints = newHitPoints }
            in
            { b | player = newPlayer }
        
        BattleEffect.ChangeMonsterHitPoints d ->
            let
                m =
                    b.monster
                
                newHitPoints =
                    boundedBy 0 m.maxHitPoints (m.hitPoints + d)
                
                newMonster =
                    { m | hitPoints = newHitPoints }
            in
            { b | monster = newMonster }

boundedBy : Int -> Int -> Int -> Int
boundedBy lower upper x =
    x
        |> min upper
        |> max lower

applyEffectsToBattle : List BattleEffect -> Battle -> Battle
applyEffectsToBattle effects m =
    List.foldl applyEffectToBattle m effects
 
type CharacterCreationError
    = MissingName
    | MissingHairStyle
    | MissingHairColor
    | MissingEyeColor
    | MissingComplexion
    | MissingHeight
    | MissingBuild

characterCreationErrorToString : CharacterCreationError -> String
characterCreationErrorToString e =
    case e of
        MissingName ->
            "Missing name"
        
        MissingHairStyle ->
            "Missing hair style"
        
        MissingHairColor ->
            "Missing hair color"
        
        MissingEyeColor ->
            "Missing eye color"
        
        MissingComplexion ->
            "Missing complexion"
        
        MissingHeight ->
            "Missing height"
        
        MissingBuild ->
            "Missing build"

characterCreationSettingsToSceneModel : CharacterCreationSettings -> Result (List CharacterCreationError) SceneModel
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
            { scene = PlayerScene
            , name = name
            , avatar = avatar
            , level = 1
            , experience = 0
            , satiety = 10
            , maxSatiety = 10
            , hitPoints = 10
            , maxHitPoints = 10
            , magicPoints = 5
            , maxMagicPoints = 5
            , attack = 1
            }
        )))))))

type Scene
    = PlayerScene
    | HomeScene
    | ExploreScene
    | ExploreDungeonScene Delve
    | BattleScene
    | BattleMonsterScene Monster
    | VictoryScene Monster Reward
    | GameOverScene

type alias Avatar =
    { hairStyle : HairStyle
    , hairColor : HairColor
    , eyeColor : EyeColor
    , complexion : Complexion
    , height : Height
    , build : Build
    }

avatarDescription : Avatar -> String
avatarDescription a =
    (Height.toString a.height) 
    ++ " and " 
    ++ (Build.toString a.build) 
    ++ " frame | "
    ++ (Complexion.toString a.complexion)
    ++ " complexion | "
    ++ (HairColor.toString a.hairColor) 
    ++ ", " 
    ++ (HairStyle.toString a.hairStyle)
    ++ " hair | "
    ++ (EyeColor.toString a.eyeColor)
    ++ " eyes"

-- MSG

type Msg
    = NoOp
    | UserSelectedPlayerScene
    | UserSelectedHomeScene
    | UserSelectedHomeRest
    | UserSelectedExploreScene
    | UserSelectedExploreDungeonScene Dungeon
    | SystemGotDungeonInitialization Dungeon (List DungeonPath.Path)
    | UserSelectedDungeonPath Delve DungeonPath.Path
    | SystemGotDungeonScene Delve DungeonScene.Scene
    | UserSelectedContinueDungeon Delve
    | SystemGotDungeonContinuation Delve (List DungeonPath.Path)
    | UserSelectedBattleScene
    | UserSelectedBattleMonsterScene Monster
    | UserSelectedBattleAction Monster Action
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
            { name = FormResult.FRBlank MissingName
            , hairStyle = FormResult.FRBlank MissingHairStyle
            , hairColor = FormResult.FRBlank MissingHairColor
            , eyeColor = FormResult.FRBlank MissingEyeColor
            , complexion = FormResult.FRBlank MissingComplexion
            , height = FormResult.FRBlank MissingHeight
            , build = FormResult.FRBlank MissingBuild
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
        
        ScenePhase sceneModel ->
            viewScenePhase sceneModel

viewCharacterCreationPhase : CharacterCreationModel -> Html Msg
viewCharacterCreationPhase model =
    let
        settingToInfo : (a -> String) -> FormResult CharacterCreationError a -> Html Msg
        settingToInfo aToString x =
            case x of
                FormResult.FRBlank _ ->
                    Html.text <| ""
                
                FormResult.FROk a ->
                    Html.text <| aToString a
                
                FormResult.FRErr e ->
                    Html.text <| characterCreationErrorToString e
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

viewScenePhase : SceneModel -> Html Msg
viewScenePhase sceneModel =
    Html.div
        []
        [ textList
            [ sceneModel.name
            , avatarDescription sceneModel.avatar
            , "LV: " ++ String.fromInt sceneModel.level
            , "EXP: " ++ String.fromInt sceneModel.experience ++ " / " ++ String.fromInt (levelUpExperience sceneModel.level)
            , "Satiety: " ++ String.fromInt sceneModel.satiety ++ " / " ++ String.fromInt sceneModel.maxSatiety
            , "HP: " ++ String.fromInt sceneModel.hitPoints ++ " / " ++ String.fromInt sceneModel.maxHitPoints
            , "MP: " ++ String.fromInt sceneModel.magicPoints ++ " / " ++ String.fromInt sceneModel.maxMagicPoints
            ]
        , case sceneModel.scene of
            BattleMonsterScene _ ->
                Html.div
                    []
                    []
            
            ExploreDungeonScene _ ->
                Html.div
                    []
                    []
            
            _ ->
                 buttonList
                    [ ( "Player", UserSelectedPlayerScene )
                    , ( "Home", UserSelectedHomeScene )
                    , ( "Shop", NoOp )
                    , ( "Town", NoOp )
                    , ( "Explore", UserSelectedExploreScene )
                    , ( "Battle", UserSelectedBattleScene )
                    ]
        , viewSceneModel sceneModel
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
    

viewSceneModel : SceneModel -> Html Msg
viewSceneModel sceneModel =
    case sceneModel.scene of
        PlayerScene ->
            textList
                [ "TODO"
                ]
        
        HomeScene ->
            buttonList
                [ ( "Rest", UserSelectedHomeRest )
                ]
        
        ExploreScene ->
            dungeonTable
                [ Dungeon.byId "beginnerscave"
                ]
        
        ExploreDungeonScene delve ->
            viewExploreDungeonScene sceneModel delve

        BattleScene ->
            monsterTable
                [ Monster.byId "gremlin"
                ]
        
        BattleMonsterScene monster ->
            viewBattleMonsterScene sceneModel monster
        
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

viewExploreDungeonScene : SceneModel -> Delve -> Html Msg
viewExploreDungeonScene sceneModel delve =
    Html.div
        []
        [ textList
            [ "Exploring: " ++ delve.dungeon.name
            , "Floor: " ++ String.fromInt delve.floor ++ " / " ++ String.fromInt delve.dungeon.depth
            ]
        , case delve.phase of
            ExplorationPhase paths ->
                pathTable delve paths
            
            ActionPhase scene ->
                Html.ul
                    []
                    [ Html.text <| DungeonScene.toString scene
                    , Html.button
                        [ Html.Events.onClick <| UserSelectedContinueDungeon delve ]
                        [ Html.text "Continue" ]
                    ]
        ]

pathTable : Delve -> List DungeonPath.Path -> Html Msg
pathTable delve paths =
    let
        pathFn path =
            Html.li
                []
                [ Html.text path.description
                , explainSceneDistribution path.sceneDistribution
                , Html.button
                    [ Html.Events.onClick <| UserSelectedDungeonPath delve path ]
                    [ Html.text "Go" ]
                ]
    in
    Html.ul
        []
        ( List.map pathFn paths )

explainSceneDistribution : Distribution DungeonScene.Scene -> Html Msg
explainSceneDistribution d =
    let
        explainOneScene ( chance, scene ) =
            String.fromFloat chance ++ "% of " ++ DungeonScene.toString scene
    in
    textList (List.map explainOneScene (d.head :: d.tail))

viewBattleMonsterScene : SceneModel -> Monster -> Html Msg
viewBattleMonsterScene sceneModel monster =
    Html.div
        []
        [ textList
            [ monster.name
            , "HP: " ++ String.fromInt monster.hitPoints
            , "Intent: Attack " ++ String.fromInt monster.attack
            ]
        , Html.ul
            []
            [ Html.li
                []
                [ Html.text <| "Attack " ++ String.fromInt sceneModel.attack
                , Html.button
                    [ Html.Events.onClick <| UserSelectedBattleAction monster (Action.byId "attack") ]
                    [ Html.text "Go" ]
                ]
            ]
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
            Html.span
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
        ( UserSelectedPlayerScene, ScenePhase sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel | scene = PlayerScene }
            in
            ( { model | phase = ScenePhase newSceneModel }, Cmd.none )
        
        ( UserSelectedHomeScene, ScenePhase sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel | scene = HomeScene }
            in
            ( { model | phase = ScenePhase newSceneModel }, Cmd.none )
        
        ( UserSelectedHomeRest, ScenePhase sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel | hitPoints = sceneModel.maxHitPoints }
            in
            ( { model | phase = ScenePhase newSceneModel }, Cmd.none )
        
        ( UserSelectedExploreScene, ScenePhase sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel | scene = ExploreScene }
            in
            ( { model | phase = ScenePhase newSceneModel }, Cmd.none )
        
        ( UserSelectedExploreDungeonScene dungeon, ScenePhase _ ) ->
            let
                pathListGenerator =
                    Random.list 3 DungeonPath.generator
                
                cmd =
                    Random.generate (SystemGotDungeonInitialization dungeon) pathListGenerator
            in
            ( model, cmd )
        
        ( SystemGotDungeonInitialization dungeon paths, ScenePhase sceneModel ) ->
            let
                delve =
                    { dungeon = dungeon
                    , floor = 1
                    , phase = ExplorationPhase paths
                    }
                
                newSceneModel =
                    { sceneModel | scene = ExploreDungeonScene delve }
            in
            ( { model | phase = ScenePhase newSceneModel }, Cmd.none )
        
        ( UserSelectedDungeonPath delve path, ScenePhase _ ) ->
            let
                cmd =
                    Random.generate (SystemGotDungeonScene delve) (Distribution.random path.sceneDistribution)
            in
            ( model, cmd )
        
        ( SystemGotDungeonScene delve scene, ScenePhase sceneModel ) ->
            let
                newDelve =
                    { delve | phase = ActionPhase scene }
                
                newSceneModel =
                    { sceneModel | scene = ExploreDungeonScene newDelve }
                
            in
            ( { model | phase = ScenePhase newSceneModel }, Cmd.none )
        
        ( UserSelectedContinueDungeon delve, ScenePhase _ ) ->
            let
                pathListGenerator =
                    Random.list 3 DungeonPath.generator

                cmd =
                    Random.generate (SystemGotDungeonContinuation delve) pathListGenerator
            in
            ( model, cmd )
        
        ( SystemGotDungeonContinuation delve paths, ScenePhase sceneModel ) ->
            let
                newDelve =
                    { delve
                        | phase = ExplorationPhase paths
                        , floor =
                            delve.floor + 1
                                |> boundedBy 1 delve.dungeon.depth
                    }
                
                newSceneModel =
                    { sceneModel | scene = ExploreDungeonScene newDelve }
                
            in 
            ( { model | phase = ScenePhase newSceneModel }, Cmd.none )
        
        ( UserSelectedBattleScene, ScenePhase sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel | scene = BattleScene }
            in
            ( { model | phase = ScenePhase newSceneModel }, Cmd.none )
        
        ( UserSelectedBattleMonsterScene monster, ScenePhase sceneModel ) ->
            let
                newSceneModel =
                    { sceneModel | scene = BattleMonsterScene monster }
            in
            ( { model | phase = ScenePhase newSceneModel }, Cmd.none )
        
        ( UserSelectedBattleAction monster action, ScenePhase sceneModel ) ->
            updateBattleAction model monster action sceneModel
        
        ( UserSelectedCharacterCreationSettingSelection selection, CharacterCreationPhase characterCreationModel ) ->
            updateCharacterCreationSettingSelection model characterCreationModel selection
        
        ( UserSelectedCharacterCreationConfirmation, CharacterCreationPhase characterCreationModel ) ->
            updateCharacterCreationConfirmation model characterCreationModel
        
        ( DevSelectedCharacterCreationConfirmation, CharacterCreationPhase characterCreationModel ) ->
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
                    { scene = PlayerScene
                    , name = "Dev"
                    , avatar = avatar
                    , level = 1
                    , experience = 0
                    , satiety = 10
                    , maxSatiety = 10
                    , hitPoints = 10
                    , maxHitPoints = 10
                    , magicPoints = 5
                    , maxMagicPoints = 5
                    , attack = 1
                    }
                
                newModel =
                    { model | phase = ScenePhase sceneModel } 
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
            checkCharacterCreationSettings characterCreationModel.settings
        
        sceneModelResult =
            characterCreationSettingsToSceneModel newSettings
        
        newModel =
            case sceneModelResult of
                Ok sceneModel ->
                    { model | phase = ScenePhase sceneModel }
                
                Err _ ->
                    { model | phase = CharacterCreationPhase { settings = newSettings }}

    in
    ( newModel, Cmd.none )

updateBattleAction : Model -> Monster -> Action -> SceneModel -> ( Model, Cmd Msg )
updateBattleAction model monster action sceneModel =
    let
        battle =
            { monster = monster
            , player = sceneModel
            }
        
        battleEffects =
            action.effects ++
                [ BattleEffect.ChangePlayerHitPoints -1
                ]
        
        newBattle =
            applyEffectsToBattle battleEffects battle
        
        newMonster =
            newBattle.monster
        
        newSceneModel =
            newBattle.player
        
        newSceneModel2 =
            if newSceneModel.hitPoints <= 0 then
                { newSceneModel | scene = GameOverScene }
            else if newMonster.hitPoints <= 0 then
                let
                    reward =
                        { experience = newMonster.experience
                        }
                in
                { newSceneModel
                    | scene = VictoryScene newMonster reward
                    , experience = newSceneModel.experience + reward.experience
                }
            else
                { newSceneModel | scene = BattleMonsterScene newMonster }
    in
    ( { model | phase = ScenePhase newSceneModel2 }, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none