module Coordinates exposing
  ( Coordinates
  , encoder
  , decoder
  , toString
  , fromString
  , adjacent
  , move
  , fake
  )

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)

import CustomDict exposing (CustomDict)

import Direction exposing (Direction)

type alias Coordinates =
  { x : Int
  , y : Int
  }

encoder : Coordinates -> Encode.Value
encoder c =
  Encode.object
    [ ( "x", Encode.int c.x )
    , ( "y", Encode.int c.y )
    ]

decoder : Decoder Coordinates
decoder =
  Decode.map2 Coordinates
    (Decode.field "x" Decode.int)
    (Decode.field "y" Decode.int)

fake : Coordinates
fake =
  { x = 0
  , y = 0
  }

toString : Coordinates -> String
toString c =
  Encode.encode 0 (encoder c)

fromString : String -> Maybe Coordinates
fromString s =
  s
    |> Decode.decodeString decoder
    |> Result.toMaybe

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