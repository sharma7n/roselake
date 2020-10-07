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
    Html.text "Hello, world!"

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none