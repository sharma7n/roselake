module CharacterCreationModel exposing
    ( CharacterCreationModel
    , generator
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