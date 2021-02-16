module Direction exposing
  ( Direction(..)
  , toString
  )

type Direction
  = North
  | East
  | South
  | West

toString : Direction -> String
toString d =
  case d of
    North ->
      "North"
    
    East ->
      "East"
    
    South ->
      "South"
    
    West ->
      "West"