module SceneModel exposing
    ( SceneModel
    , characterCreationSettingsToSceneModel
    , applyEffectsToSceneModel
    )

import FormResult
import Util

import CharacterCreationError
import CharacterCreationSettings exposing (CharacterCreationSettings)

import Action exposing (Action)
import Avatar exposing (Avatar)
import Effect exposing (Effect)
import Inventory exposing (Inventory)
import Weapon exposing (Weapon)

type alias SceneModel =
    { name : String
    , avatar : Avatar
    , gold : Int
    , inventory : Inventory
    , level : Int
    , experience : Int
    , freeAbilityPoints : Int
    , totalAbilityPoints : Int
    , satiety : Int
    , maxSatiety : Int
    , hitPoints : Int
    , maxHitPoints : Int
    , magicPoints : Int
    , maxMagicPoints : Int
    , attack : Int
    , agility : Int
    , actions : List Action
    , equippedWeapon : Maybe Weapon
    }

applyEffectToSceneModel : Effect -> SceneModel -> SceneModel
applyEffectToSceneModel effect m =
    case effect of
        Effect.ChangeLevel d ->
            { m | level = max 1 (m.level + d) }
        
        Effect.ChangeExperience d ->
            { m | experience = max 0 (m.experience + d) }
        
        Effect.ChangeSatiety d ->
            { m | satiety = Util.boundedBy 0 m.maxSatiety (m.satiety + d) }
        
        Effect.ChangeMaxSatiety d ->
            let
                newMaxSatiety =
                    max 1 (m.maxSatiety + d)
            in
            { m
                | maxSatiety = newMaxSatiety
                , satiety = Util.boundedBy 0 newMaxSatiety m.satiety
            }
        
        Effect.ChangeHitPoints d ->
            { m | hitPoints = Util.boundedBy 0 m.maxHitPoints (m.hitPoints + d) }
        
        Effect.ChangeMaxHitPoints d ->
            let
                newMaxHitPoints =
                    max 1 (m.maxHitPoints + d)
            in
            { m
                | maxHitPoints = newMaxHitPoints
                , hitPoints = Util.boundedBy 0 newMaxHitPoints m.hitPoints
            }
        
        Effect.ChangeMagicPoints d ->
            { m | magicPoints = Util.boundedBy 0 m.maxMagicPoints (m.magicPoints + d) }

        Effect.ChangeMaxMagicPoints d ->
            let
                newMaxMagicPoints =
                    max 1 (m.maxMagicPoints + d)
            in
            { m
                | maxMagicPoints = newMaxMagicPoints
                , magicPoints = Util.boundedBy 0 newMaxMagicPoints m.magicPoints
            }
        
        Effect.ChangeAttack d ->
            { m | attack = max 0 (m.attack + d) }
        
        _ ->
            m

applyEffectsToSceneModel : List Effect -> SceneModel -> SceneModel
applyEffectsToSceneModel effects m =
    List.foldl applyEffectToSceneModel m effects

characterCreationSettingsToSceneModel : CharacterCreationSettings -> Result (List CharacterCreationError.Error) SceneModel
characterCreationSettingsToSceneModel settings =
    FormResult.toValidation settings.name
        |> Result.andThen (\name -> FormResult.toValidation settings.hairStyle
        |> Result.andThen (\hairStyle -> FormResult.toValidation settings.hairColor
        |> Result.andThen (\hairColor -> FormResult.toValidation settings.eyeColor
        |> Result.andThen (\eyeColor -> FormResult.toValidation settings.complexion
        |> Result.andThen (\complexion -> FormResult.toValidation settings.height
        |> Result.andThen (\height -> FormResult.toValidation settings.build
        |> Result.andThen (\build -> Ok <|
            let
                avatar =
                    { hairStyle = hairStyle
                    , hairColor = hairColor
                    , eyeColor = eyeColor
                    , complexion = complexion
                    , height = height
                    , build = build
                    }
            in
            { name = name
            , avatar = avatar
            , gold = 0
            , inventory = Inventory.new
            , level = 1
            , experience = 0
            , freeAbilityPoints = 0
            , totalAbilityPoints = 0
            , satiety = 10
            , maxSatiety = 10
            , hitPoints = 10
            , maxHitPoints = 10
            , magicPoints = 5
            , maxMagicPoints = 5
            , attack = 1
            , agility = 1
            , actions =
                [ Action.byId "attack"
                , Action.byId "fireball"
                ]
            , equippedWeapon = Just <| Weapon.byId "sword"
            }
        )))))))