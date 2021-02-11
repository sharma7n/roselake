module Coordinates exposing
  ( Coordinates
  , toString
  , newDict
  )

import CustomDict exposing (CustomDict)

type alias Coordinates =
  { x : Int
  , y : Int
  }

toString : Coordinates -> String
toString c =
  String.fromInt c.x ++ "," ++ String.fromInt c.y

newDict : CustomDict Coordinates a
newDict =
  CustomDict.new toString