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