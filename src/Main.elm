module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events

-- MODEL

type alias Model =
    { scene : Scene
    }

type Scene
    = PlayerScene

-- MSG

type Msg
    = NoOp
    | UserSelectedPlayerScene

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
        initModel =
            { scene = PlayerScene
            }
    in
    ( initModel, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
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
            , ( "Home", NoOp )
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

viewScene : Scene -> Html Msg
viewScene scene =
    case scene of
        PlayerScene ->
            textList
                [ "STR: 1"
                , "DEF: 1"
                , "AGL: 1"
                ]

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none