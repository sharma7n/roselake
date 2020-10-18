module Main exposing (main)

import Browser

import Model exposing (Model)
import Msg exposing (Msg)

import Init
import View
import Update
import Subscriptions

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = Init.init
        , view = View.view
        , update = Update.update
        , subscriptions = Subscriptions.subscriptions
        }