module Coordinates exposing
  ( Coordinates
  , toString
  , fromString
  , newDict
  , adjacent
  , move
  )

import Parser exposing (Parser)

import CustomDict exposing (CustomDict)

import Direction exposing (Direction)

type alias Coordinates =
  { x : Int
  , y : Int
  }

toString : Coordinates -> String
toString c =
  String.fromInt c.x ++ "," ++ String.fromInt c.y

fromString : String -> Maybe Coordinates
fromString s =
  let
    coordinatesParser =
      Parser.succeed Coordinates
        Parser.(|=) Parser.int
        Parser.(|=) (Parser.symbol ",")
        Parser.(|=) Parser.int
  in
  Parser.run coordinatesParser s
    |> Result.toMaybe

newDict : CustomDict Coordinates a
newDict =
  CustomDict.new toString

adjacent : Coordinates -> Coordinates -> Bool
adjacent p1 p2 =
  ( p1.x == p2.x && (p1.y - p2.y)*(p1.y - p2.y) == 1 )
  ||
  ( p1.y == p2.y && (p1.x - p2.x)*(p1.x - p2.x) == 1 )

move : Direction -> Coordinates -> Coordinates
move dir c =
  case dir of
    Direction.North ->
      { c | y = c.y + 1 }
    
    Direction.East ->
      { c | x = c.x + 1 }
    
    Direction.South ->
      { c | y = c.y - 1 }
    
    Direction.West ->
      { c | x = c.x - 1 }