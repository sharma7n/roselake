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

type alias SceneModel =
    { scene : Scene
    , name : String
    , avatar : Avatar
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
            }
        )))))))

type Scene
    = PlayerScene
    | HomeScene

type alias Avatar =
    { hairStyle : HairStyle
    , hairColor : HairColor
    , eyeColor : EyeColor
    , complexion : Complexion
    , height : Height
    , build : Build
    }

-- MSG

type Msg
    = NoOp
    | UserSelectedPlayerScene
    | UserSelectedHomeScene
    | UserSelectedCharacterCreationSettingSelection CharacterCreationSettingSelection
    | UserSelectedCharacterCreationConfirmation

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
        , Html.button [] [ Html.text "Create" ]
        ]

viewScenePhase : SceneModel -> Html Msg
viewScenePhase sceneModel =
    Html.div
        []
        [ textList
            [ sceneModel.name
            , "Avatar"
            , "LV: 1"
            , "EXP: 0"
            , "States: []"
            , "Satiety: 10 / 10"
            , "HP: 10 / 10"
            , "MP: 5 / 5"
            ]
        , buttonList
            [ ( "Player", UserSelectedPlayerScene )
            , ( "Home", UserSelectedHomeScene )
            , ( "Shop", NoOp )
            , ( "Town", NoOp )
            , ( "Explore", NoOp )
            , ( "Battle", NoOp )
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
            Html.div
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
        
        ( UserSelectedCharacterCreationSettingSelection selection, CharacterCreationPhase characterCreationModel ) ->
            updateCharacterCreationSettingSelection model characterCreationModel selection
        
        ( UserSelectedCharacterCreationConfirmation, CharacterCreationPhase characterCreationModel ) ->
            updateCharacterCreationConfirmation model characterCreationModel
        
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
        sceneModelResult =
            characterCreationSettingsToSceneModel characterCreationModel.settings
        
        newModel =
            case sceneModelResult of
                Ok sceneModel ->
                    { model | phase = ScenePhase sceneModel }
                
                Err _ ->
                    model

    in
    ( newModel, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none