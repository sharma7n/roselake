module CharacterCreationError exposing
    ( Error(..)
    , toString
    )

type Error
    = MissingName
    | MissingHairStyle
    | MissingHairColor
    | MissingEyeColor
    | MissingComplexion
    | MissingHeight
    | MissingBuild
    | MissingStartingWeapon
    | MissingClass

toString : Error -> String
toString e =
    case e of
        MissingName ->
            "Missing name"
        
        MissingHairStyle ->
            "Missing hair style"
        
        MissingHairColor ->
            "Missing hair color"
        
        MissingEyeColor ->
            "Missing eye color"
        
        MissingComplexion ->
            "Missing complexion"
        
        MissingHeight ->
            "Missing height"
        
        MissingBuild ->
            "Missing build"
        
        MissingStartingWeapon ->
            "Missing starting weapon"
        
        MissingClass ->
            "Missing class"