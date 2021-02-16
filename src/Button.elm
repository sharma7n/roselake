module Button exposing
  ( button
  , coloredButton
  , disabledButton
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

button : String -> msg -> Element msg
button labelText msg =
    Element.Input.button
        [ Element.Background.color Palette.lightGray
        , Element.padding 10
        -- , Element.Border.rounded 10
        , Element.Border.width 1
        ]
        { onPress = Just msg
        , label = Element.text labelText
        }

coloredButton : Element.Color -> String -> msg -> Element msg
coloredButton color labelText msg =
    Element.Input.button
        [ Element.Background.color color
        , Element.padding 10
        -- , Element.Border.rounded 10
        , Element.Border.width 1
        ]
        { onPress = Just msg
        , label = Element.text labelText
        }

disabledButton : String -> Element msg
disabledButton labelText =
    Element.Input.button
        [ Element.padding 10
        -- , Element.Border.rounded 10
        ]
        { onPress = Nothing
        , label = Element.text labelText
        }