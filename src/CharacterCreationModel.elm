module CharacterCreationModel exposing
    ( CharacterCreationModel
    , generator
    )

import Random

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
        |> Random.andThen (\settings ->
            Random.constant <|
                { settings = settings
                , attributePoints = 25
                , strength = 1
                , vitality = 1
                , agility = 1
                , intellect = 1
                , charisma = 1
                }
        )