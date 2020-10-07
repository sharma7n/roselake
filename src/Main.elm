module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events

import HairStyle exposing (HairStyle)
import HairColor exposing (HairColor)
import EyeColor exposing (EyeColor)
import Complexion exposing (Complexion)
import Height exposing (Height)
import Build exposing (Build)

-- MODEL

type alias Model =
    { scene : Scene
    , phase : Phase
    , characterCreationSettings : CharacterCreationSettings
    }

type Scene
    = PlayerScene
    | HomeScene

type Phase
    = CharacterCreationPhase
    | MainPhase

type alias CharacterCreationSettings =
    { name : Maybe String
    , hairStyle : Maybe HairStyle
    , hairColor : Maybe HairColor
    , eyeColor : Maybe EyeColor
    , complexion : Maybe Complexion
    , height : Maybe Height
    , build : Maybe Build
    }

-- MSG

type Msg
    = NoOp
    | UserSelectedPlayerScene
    | UserSelectedHomeScene

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
        
        initModel =
            { scene = PlayerScene
            , phase = CharacterCreationPhase
            , characterCreationSettings = initCharacterCreationSettings
            }
    in
    ( initModel, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    case model.phase of
        CharacterCreationPhase ->
            viewCharacterCreationPhase model
        
        MainPhase ->
            viewMainPhase model

viewCharacterCreationPhase : Model -> Html Msg
viewCharacterCreationPhase model =
    Html.div
        []
        [ Html.text "Create Character"
        , formList
            [ ( "Name", Html.input [] [ Html.text <| Maybe.withDefault "" model.characterCreationSettings.name ] )
            , ( "Hair Style", Html.select [] (List.map (Html.text << HairStyle.toString) HairStyle.all) )
            , ( "Hair Color", Html.select [] (List.map (Html.text << HairColor.toString) HairColor.all) )
            , ( "Eye Color", Html.select [] (List.map (Html.text << EyeColor.toString) EyeColor.all) )
            , ( "Complexion", Html.select [] (List.map (Html.text << Complexion.toString) Complexion.all) )
            , ( "Height", Html.select [] (List.map (Html.text << Height.toString) Height.all) )
            , ( "Build", Html.select [] (List.map (Html.text << Build.toString) Build.all) )
            ]
        , Html.button [] [ Html.text "Create" ]
        ]

viewMainPhase : Model -> Html Msg
viewMainPhase model =
    Html.div
        []
        [ textList
            [ "Name"
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
        , viewScene model.scene
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

formList : List ( String, Html Msg ) -> Html Msg
formList items =
    let
        itemFn ( label, form ) =
            Html.li
                []
                [ Html.text label
                , form
                ]
    in
    Html.ul
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
    case msg of
        UserSelectedPlayerScene ->
            ( { model | scene = PlayerScene }, Cmd.none )
        
        UserSelectedHomeScene ->
            ( { model | scene = HomeScene }, Cmd.none )
        
        _ ->
            ( model, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none