module CharacterCreationSettings exposing
    ( CharacterCreationSettings
    , check
    , generator
    )

import Random

import CharacterCreationError
import FormResult exposing (FormResult)
import HairStyle exposing (HairStyle)
import HairColor exposing (HairColor)
import EyeColor exposing (EyeColor)
import Complexion exposing (Complexion)
import Height exposing (Height)
import Build exposing (Build)
import Weapon exposing (Weapon)
import Name
import Essentia exposing (Essentia)

type alias CharacterCreationSettings =
    { name : FormResult CharacterCreationError.Error String
    , hairStyle : FormResult CharacterCreationError.Error HairStyle
    , hairColor : FormResult CharacterCreationError.Error HairColor
    , eyeColor : FormResult CharacterCreationError.Error EyeColor
    , complexion : FormResult CharacterCreationError.Error Complexion
    , height : FormResult CharacterCreationError.Error Height
    , build : FormResult CharacterCreationError.Error Build
    , startingWeapon : FormResult CharacterCreationError.Error Weapon
    , startingEssentia : FormResult CharacterCreationError.Error Essentia
    }

check : CharacterCreationSettings -> CharacterCreationSettings
check settings =
    let
        checkName name =
            case name of
                FormResult.FROk okName ->
                    if okName == "" then 
                        FormResult.FRErr CharacterCreationError.MissingName 
                    else   
                        FormResult.FROk okName
                
                _ ->
                    FormResult.FRErr CharacterCreationError.MissingName
    in
    { name = checkName settings.name
    , hairStyle = FormResult.check settings.hairStyle
    , hairColor = FormResult.check settings.hairColor
    , eyeColor = FormResult.check settings.eyeColor
    , complexion = FormResult.check settings.complexion
    , height = FormResult.check settings.height
    , build = FormResult.check settings.build
    , startingWeapon = FormResult.check settings.startingWeapon
    , startingEssentia = FormResult.check settings.startingEssentia
    }

generator : Random.Generator CharacterCreationSettings
generator =
    Name.generator
        |> Random.andThen (\name -> HairStyle.generator
        |> Random.andThen (\hairStyle -> HairColor.generator
        |> Random.andThen (\hairColor -> EyeColor.generator
        |> Random.andThen (\eyeColor -> Complexion.generator
        |> Random.andThen (\complexion -> Height.generator
        |> Random.andThen (\height -> Build.generator
        |> Random.andThen (\build -> Weapon.generator
        |> Random.andThen (\startingWeapon -> Essentia.generator
        |> Random.andThen (\startingEssentia -> Random.constant <|
            { name = FormResult.FROk name
            , hairStyle = FormResult.FROk hairStyle
            , hairColor = FormResult.FROk hairColor
            , eyeColor = FormResult.FROk eyeColor
            , complexion = FormResult.FROk complexion
            , height = FormResult.FROk height
            , build = FormResult.FROk build
            , startingWeapon = FormResult.FROk startingWeapon
            , startingEssentia = FormResult.FROk startingEssentia
            }
        )))))))))