module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode

import FormResult exposing (FormResult)
import HairStyle exposing (HairStyle)
import HairColor exposing (HairColor)
import EyeColor exposing (EyeColor)
import Complexion exposing (Complexion)
import Height exposing (Height)
import Build exposing (Build)

import Action exposing (Action)
import Monster exposing (Monster)

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
    }

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
            }
        )))))))

type Scene
    = PlayerScene
    | HomeScene
    | BattleScene
    | BattleMonsterScene Monster

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
            , "EXP: " ++ String.fromInt sceneModel.experience
            , "Satiety: " ++ String.fromInt sceneModel.satiety ++ " / " ++ String.fromInt sceneModel.maxSatiety
            , "HP: " ++ String.fromInt sceneModel.hitPoints ++ " / " ++ String.fromInt sceneModel.maxHitPoints
            , "MP: " ++ String.fromInt sceneModel.magicPoints ++ " / " ++ String.fromInt sceneModel.maxMagicPoints
            ]
        , buttonList
            [ ( "Player", UserSelectedPlayerScene )
            , ( "Home", UserSelectedHomeScene )
            , ( "Shop", NoOp )
            , ( "Town", NoOp )
            , ( "Explore", NoOp )
            , ( "Battle", UserSelectedBattleScene )
            ]
        , viewScene sceneModel.scene
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
    

viewScene : Scene -> Html Msg
viewScene scene =
    case scene of
        PlayerScene ->
            textList
                [ "STR: 1"
                , "DEF: 1"
                , "AGI: 1"
                ]
        
        HomeScene ->
            textList
                [ "Rest"
                ]
        
        BattleScene ->
            monsterTable
                [ Monster.byId "gremlin"
                ]
        
        BattleMonsterScene monster ->
            Html.div
                []
                [ textList
                    [ monster.name
                    , "HP: " ++ String.fromInt monster.hitPoints
                    ]
                , Html.ul
                    []
                    [ Html.li
                        []
                        [ Html.button
                            [ Html.Events.onClick <| UserSelectedBattleAction monster (Action.byId "attack") ]
                            [ Html.text "Attack" ]
                        ]
                    ]
                ]

monsterTable : List Monster -> Html Msg
monsterTable monsters =
    let
        monsterFn monster =
            Html.span
                []
                [ Html.text <| monster.name
                , Html.text <| String.fromInt monster.hitPoints
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
            let
                newMonster =
                    { monster | hitPoints = monster.hitPoints - 1 }
                
                newSceneModel =
                    { sceneModel | scene = BattleMonsterScene newMonster }
            in
            ( { model | phase = ScenePhase newSceneModel }, Cmd.none )
        
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

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none