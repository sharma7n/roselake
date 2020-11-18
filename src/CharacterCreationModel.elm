module CharacterCreationModel exposing
    ( CharacterCreationModel
    , generator
    , getAttribute
    , modifyAttribute
    , modifyAttributePoints
    )

import Random

import Attribute exposing (Attribute)
import CharacterCreationSettings exposing (CharacterCreationSettings)

type alias CharacterCreationModel =
    { settings : CharacterCreationSettings
    , attributePoints : Int
    , strength : Int
    , vitality : Int
    , agility : Int
    , intellect : Int
    , charisma : Int
    }

getAttribute : Attribute -> CharacterCreationModel -> Int
getAttribute attr c =
    case attr of
        Attribute.Strength ->
            c.strength
        
        Attribute.Vitality ->
            c.vitality
        
        Attribute.Agility ->
            c.agility
        
        Attribute.Intellect ->
            c.intellect
        
        Attribute.Charisma ->
            c.charisma

modifyAttribute : Attribute -> Int -> CharacterCreationModel -> CharacterCreationModel
modifyAttribute attr d c =
    case attr of
        Attribute.Strength ->
            { c | strength = c.strength + d }
        
        Attribute.Vitality ->
            { c | vitality = c.vitality + d }
        
        Attribute.Agility ->
            { c | agility = c.agility + d }
        
        Attribute.Intellect ->
            { c | intellect = c.intellect + d }
        
        Attribute.Charisma ->
            { c | charisma = c.charisma + d }

modifyAttributePoints : Int -> CharacterCreationModel -> CharacterCreationModel
modifyAttributePoints d c =
    { c | attributePoints = c.attributePoints + d }

generator : Random.Generator CharacterCreationModel
generator =
    CharacterCreationSettings.generator
        |> Random.andThen (\settings -> Attribute.collectionGenerator
        |> Random.andThen (\collection ->
            Random.constant <|
                { settings = settings
                , attributePoints = 0
                , strength = collection.strength
                , vitality = collection.vitality
                , agility = collection.agility
                , intellect = collection.intellect
                , charisma = collection.charisma
                }
        ))