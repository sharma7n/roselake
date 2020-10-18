module Init exposing
    ( init
    )

import FormResult

import CharacterCreationError
import CharacterCreationSettings exposing (CharacterCreationSettings)

import Phase exposing (Phase)

import Model exposing (Model)
import Msg exposing (Msg)

init : flags -> ( Model, Cmd Msg )
init _ =
    let
        initCharacterCreationSettings =
            { name = FormResult.FRBlank CharacterCreationError.MissingName
            , hairStyle = FormResult.FRBlank CharacterCreationError.MissingHairStyle
            , hairColor = FormResult.FRBlank CharacterCreationError.MissingHairColor
            , eyeColor = FormResult.FRBlank CharacterCreationError.MissingEyeColor
            , complexion = FormResult.FRBlank CharacterCreationError.MissingComplexion
            , height = FormResult.FRBlank CharacterCreationError.MissingHeight
            , build = FormResult.FRBlank CharacterCreationError.MissingBuild
            }
        
        initCharacterCreationModel =
            { settings = initCharacterCreationSettings
            }
        
        initModel =
            { phase = Phase.CharacterCreationPhase initCharacterCreationModel
            }
    in
    ( initModel, Cmd.none )