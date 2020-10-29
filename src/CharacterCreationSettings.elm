module CharacterCreationSettings exposing
    ( CharacterCreationSettings
    , check
    )

import CharacterCreationError
import FormResult exposing (FormResult)
import HairStyle exposing (HairStyle)
import HairColor exposing (HairColor)
import EyeColor exposing (EyeColor)
import Complexion exposing (Complexion)
import Height exposing (Height)
import Build exposing (Build)
import Weapon exposing (Weapon)

type alias CharacterCreationSettings =
    { name : FormResult CharacterCreationError.Error String
    , hairStyle : FormResult CharacterCreationError.Error HairStyle
    , hairColor : FormResult CharacterCreationError.Error HairColor
    , eyeColor : FormResult CharacterCreationError.Error EyeColor
    , complexion : FormResult CharacterCreationError.Error Complexion
    , height : FormResult CharacterCreationError.Error Height
    , build : FormResult CharacterCreationError.Error Build
    , startingWeapon : FormResult CharacterCreationError.Error Weapon
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
    }