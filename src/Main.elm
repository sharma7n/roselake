module Main exposing (main)

import Browser
import Html exposing (Html)

-- MODEL

type alias Model =
    {}

-- MSG

type Msg
    = NoOp

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
    ( {}, Cmd.none )

-- VIEW

view : model -> Html Msg
view _ =
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
        , textList
            [ "Player"
            , "Home"
            , "Shop"
            , "Town"
            , "Explore"
            , "Battle"
            ]
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

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none