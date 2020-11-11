module Attribute exposing
    ( Attribute(..)
    , generator
    , toString
    , toShortString
    , Collection
    , collectionGenerator
    )

import Random

import Util

type Attribute
    = Strength
    | Vitality
    | Agility
    | Intellect
    | Charisma

fold : a -> a -> a -> a -> a -> Attribute -> a
fold strength vitality agility intellect charisma a =
    case a of
        Strength ->
            strength
        
        Vitality ->
            vitality
        
        Agility ->
            agility
        
        Intellect ->
            intellect
        
        Charisma ->
            charisma

toString : Attribute -> String
toString =
    fold "Strength" "Vitality" "Agility" "Intellect" "Charisma"

toShortString : Attribute -> String
toShortString =
    fold "STR" "VIT" "AGI" "INT" "CHA"

generator : Random.Generator Attribute
generator =
    Random.uniform
        Strength
        [ Vitality
        , Agility
        , Intellect
        , Charisma
        ]

type alias Collection =
    { strength : Int
    , vitality : Int
    , agility : Int
    , intellect : Int
    , charisma : Int
    }

newCollection : Int -> Int -> Int -> Int -> Int -> Collection
newCollection strength vitality agility intellect charisma =
    { strength = strength
    , vitality = vitality
    , agility = agility
    , intellect = intellect
    , charisma = charisma
    }

collectionGenerator : Random.Generator Collection
collectionGenerator =
    Util.forCount 20 (\randomCollection ->
        randomCollection
            |> Random.andThen (\collection -> generator
            |> Random.andThen (\attribute -> Random.constant <|
                updateOneAttributeCollection attribute collection
            ))
    ) (Random.constant <| newCollection 1 1 1 1 1)

updateOneAttributeCollection : Attribute -> Collection -> Collection
updateOneAttributeCollection attr c =
    case attr of
        Strength ->
            { c | strength = c.strength + 1 }
        
        Vitality ->
            { c | vitality = c.vitality + 1 }
        
        Agility ->
            { c | agility = c.agility + 1 }
        
        Intellect ->
            { c | intellect = c.intellect + 1 }
        
        Charisma ->
            { c | charisma = c.charisma + 1 }