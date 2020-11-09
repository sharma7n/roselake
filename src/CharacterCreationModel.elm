module CharacterCreationModel exposing
    ( CharacterCreationModel
    , generator
    )

import Random

import CharacterCreationSettings exposing (CharacterCreationSettings)

type alias CharacterCreationModel =
    { settings : CharacterCreationSettings
    , attributePoints : Int
    }

generator : Random.Generator CharacterCreationModel
generator =
    CharacterCreationSettings.generator
        |> Random.andThen (\settings ->
            Random.constant <|
                { settings = settings
                , attributePoints = 25
                }
        )