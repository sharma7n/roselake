module NodeType exposing 
  ( NodeType(..)
  , generator
  , toString
  )

import Random

import Map exposing (Map)
import Event exposing (Event)
import Shop exposing (Shop)
import MonsterTemplate exposing (MonsterTemplate)

type NodeType
  = Null
  | Land
  | Event Event
  | Shop Shop
  | Inn Int
  | SavePoint
  | MonsterNest MonsterTemplate
  | Goal

generator : Map -> Random.Generator NodeType
generator m =
  Event.generator
    |> Random.andThen (\event ->
      Shop.generator
        |> Random.andThen (\shop ->
          MonsterTemplate.generator
            |> Random.andThen (\monsterTemplate ->
              Random.weighted
                ( 0, Null )
                [ ( 70, Land )
                , ( 18, Event event )
                , ( 2, Shop shop )
                , ( 2, Inn 10 )
                , ( 1, SavePoint )
                , ( 1, MonsterNest monsterTemplate )
                , ( 1, Goal )
                ]
            )
        )
    )

toString : NodeType -> String
toString t =
  case t of
    Null ->
      "Null"
    
    Land ->
      "Land"
    
    Event event ->
      "Event: " ++ event.name
     
    Shop shop ->
      "Shop: " ++ shop.name
    
    Inn i ->
      "Inn: " ++ String.fromInt i
    
    SavePoint ->
      "Save Point"
    
    MonsterNest monsterTemplate ->
      "Monster Nest: " ++ monsterTemplate.name
    
    Goal ->
      "Goal"