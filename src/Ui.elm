module Ui exposing
  ( column
  , row
  )

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Keyed
import Element.Lazy
import Element.Region

import Palette

column : List ( Element msg ) -> Element msg
column xs =
  Element.column
    [ Element.padding 10
    , Element.spacing 10
    , Element.Background.color Palette.lightBlue
    ]
    xs

row : List ( Element msg ) -> Element msg
row xs =
  Element.row
    [ Element.padding 10
    , Element.spacing 10
    , Element.Background.color Palette.lightPurple
    ]
    xs