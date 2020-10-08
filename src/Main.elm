module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode

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
    { name : Maybe String
    , hairStyle : Maybe HairStyle
    , hairColor : Maybe HairColor
    , eyeColor : Maybe EyeColor
    , complexion : Maybe Complexion
    , height : Maybe Height
    , build : Maybe Build
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

characterCreationSettingsToSceneModel : CharacterCreationSettings -> Result (List CharacterCreationError) SceneModel
characterCreationSettingsToSceneModel settings =
    maybeToValidation MissingName settings.name
        |> Result.andThen (\name -> maybeToValidation MissingHairStyle settings.hairStyle
        |> Result.andThen (\hairStyle -> maybeToValidation MissingHairColor settings.hairColor
        |> Result.andThen (\hairColor -> maybeToValidation MissingEyeColor settings.eyeColor
        |> Result.andThen (\eyeColor -> maybeToValidation MissingComplexion settings.complexion
        |> Result.andThen (\complexion -> maybeToValidation MissingHeight settings.height
        |> Result.andThen (\height -> maybeToValidation MissingBuild settings.build
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

maybeToValidation : e -> Maybe a -> Result (List e) a
maybeToValidation error maybe =
    case maybe of
        Just x ->
            Ok x
        
        Nothing ->
            Err <| List.singleton error

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
            { name = Nothing
            , hairStyle = Nothing
            , hairColor = Nothing
            , eyeColor = Nothing
            , complexion = Nothing
            , height = Nothing
            , build = Nothing
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
    Html.div
        []
        [ Html.text "Create Character"
        , formList
            [ ( "Name"
              , Html.input [ Html.Events.onInput (UserSelectedCharacterCreationSettingSelection << NameSelection) ] []
              , Maybe.withDefault "" model.settings.name 
              )
            , ( "Hair Style"
              , radioButtons HairStyle.toString (UserSelectedCharacterCreationSettingSelection << HairStyleSelection) HairStyle.all model.settings.hairStyle
              , Maybe.withDefault "" (Maybe.map HairStyle.toString model.settings.hairStyle)
              )
            , ( "Hair Color"
              , radioButtons HairColor.toString (UserSelectedCharacterCreationSettingSelection << HairColorSelection) HairColor.all model.settings.hairColor
              , Maybe.withDefault "" (Maybe.map HairColor.toString model.settings.hairColor)
              )
            , ( "Eye Color"
              , radioButtons EyeColor.toString (UserSelectedCharacterCreationSettingSelection << EyeColorSelection) EyeColor.all model.settings.eyeColor
              , Maybe.withDefault "" (Maybe.map EyeColor.toString model.settings.eyeColor)
              )
            , ( "Complexion"
              , radioButtons Complexion.toString (UserSelectedCharacterCreationSettingSelection << ComplexionSelection) Complexion.all model.settings.complexion
              , Maybe.withDefault "" (Maybe.map Complexion.toString model.settings.complexion)
              )
            , ( "Height"
              , radioButtons Height.toString (UserSelectedCharacterCreationSettingSelection << HeightSelection) Height.all model.settings.height
              , Maybe.withDefault "" (Maybe.map Height.toString model.settings.height)
              )
            , ( "Build"
              , radioButtons Build.toString (UserSelectedCharacterCreationSettingSelection << BuildSelection) Build.all model.settings.build
              , Maybe.withDefault "" (Maybe.map Build.toString model.settings.build)
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

formList : List ( String, Html Msg, String ) -> Html Msg
formList items =
    let
        itemFn ( label, form, selectedLabel ) =
            Html.li
                []
                [ Html.text label
                , form
                , Html.text selectedLabel
                ]
    in
    Html.ul
        []
        ( List.map itemFn items )

radioButtons : (a -> String) -> (a -> Msg) -> List a -> Maybe a -> Html Msg
radioButtons toString toMsg items currentItem =
    let     
        itemFn item =
            Html.div
                []
                [ Html.input
                    [ Html.Attributes.type_ "radio"
                    , Html.Attributes.checked (Just item == currentItem)
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
                    { settings | name = Just name }
                
                HairStyleSelection hairStyle ->
                    { settings | hairStyle = Just hairStyle }
                
                HairColorSelection hairColor ->
                    { settings | hairColor = Just hairColor }
                
                EyeColorSelection eyeColor ->
                    { settings | eyeColor = Just eyeColor }
                
                ComplexionSelection complexion ->
                    { settings | complexion = Just complexion }
                
                HeightSelection height ->
                    { settings | height = Just height }
                
                BuildSelection build ->
                    { settings | build = Just build }
    
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