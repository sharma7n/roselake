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

adjacent : Coordinates -> Coordinates -> Bool
adjacent p1 p2 =
  ( p1.x == p2.x && (p1.y - p2.y)*(p1.y - p2.y) == 1 )
  ||
  ( p1.y == p2.y && (p1.x - p2.x)*(p1.x - p2.x) == 1 )