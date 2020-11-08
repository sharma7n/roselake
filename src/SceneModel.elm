module SceneModel exposing
    ( SceneModel
    , characterCreationSettingsToSceneModel
    , applyEffectsToSceneModel
    , applyReward
    , completeBattle
    )

import Set exposing (Set)

import FormResult
import Util

import CharacterCreationError
import CharacterCreationSettings exposing (CharacterCreationSettings)
import HairStyle exposing (HairStyle)
import HairColor exposing (HairColor)
import EyeColor exposing (EyeColor)
import Complexion exposing (Complexion)
import Height exposing (Height)
import Build exposing (Build)


import Action exposing (Action)
import Passive exposing (Passive)
import ActionState exposing (ActionState)
import Armor exposing (Armor)
import Avatar exposing (Avatar)
import Effect exposing (Effect)
import Essentia exposing (Essentia)
import Inventory exposing (Inventory)
import Reward exposing (Reward)
import Weapon exposing (Weapon)

import EssentiaContainer exposing (EssentiaContainer)
import StatusSet exposing (StatusSet)

type alias SceneModel =
    { name : String
    , avatar : Avatar
    , gold : Int
    , inventory : Inventory
    , essentia : List Essentia
    , level : Int
    , experience : Int
    , freeAbilityPoints : Int
    , totalAbilityPoints : Int
    , learned : Set String
    , learnedPassives : Set String
    , satiety : Int
    , maxSatiety : Int
    , hitPoints : Int
    , maxHitPoints : Int
    , magicPoints : Int
    , maxMagicPoints : Int
    , actionPoints : Int
    , maxActionPoints : Int
    , vitality : Int
    , attack : Int
    , magic : Int
    , defense : Int
    , agility : Int
    , actions : List Action
    , passives : List Passive
    , equippedWeapon : Maybe Weapon
    , equippedArmor : Maybe Armor
    , essentiaContainer : EssentiaContainer
    , statusSet : StatusSet
    , actionStates : List ActionState
    , block : Int
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
        |> Result.andThen (\build -> FormResult.toValidation settings.startingWeapon
        |> Result.andThen (\startingWeapon -> FormResult.toValidation settings.startingEssentia
        |> Result.andThen (\startingEssentia -> Ok <|
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
            , essentia = []
            , level = 1
            , experience = 0
            , freeAbilityPoints = 0
            , totalAbilityPoints = 0
            , learned = Set.empty
            , learnedPassives = Set.empty
            , satiety = 10
            , maxSatiety = 10
            , hitPoints = 10
            , maxHitPoints = 10
            , magicPoints = 5
            , maxMagicPoints = 5
            , actionPoints = 3
            , maxActionPoints = 3
            , vitality = 1
            , attack = 1
            , magic = 1
            , defense = 0
            , agility = 1
            , actions = initActions (Just startingWeapon) Set.empty
            , passives = initPassives Set.empty
            , equippedWeapon = Just startingWeapon
            , equippedArmor = Just <| Armor.byId "cotton-shirt"
            , essentiaContainer = 
                EssentiaContainer.new
                    |> EssentiaContainer.setSlot EssentiaContainer.Index1 startingEssentia
            , statusSet = StatusSet.empty
            , actionStates = initActionStates <| initActions (Just startingWeapon) Set.empty
            , block = 0
            }
        )))))))))

applyReward : Reward -> SceneModel -> SceneModel
applyReward reward m =
    { m
        | experience = m.experience + reward.experience
        , gold = m.gold + reward.gold
        , freeAbilityPoints = m.freeAbilityPoints + reward.abilityPoints
        , totalAbilityPoints = m.totalAbilityPoints + reward.abilityPoints
        , inventory =
            m.inventory
                |> (\i -> List.foldl (Util.uncurry Inventory.modifyItemQuantity) i reward.items)
                |> (\i -> List.foldl (Util.uncurry Inventory.modifyWeaponQuantity) i reward.weapons)
                |> (\i -> List.foldl (Util.uncurry Inventory.modifyArmorQuantity) i reward.armors)
    }

initActions : Maybe Weapon -> Set String -> List Action
initActions equippedWeapon learned =
    let
        baseActions =
            case equippedWeapon of
                Just weapon ->
                    weapon.actions
                
                Nothing ->
                    [ Action.byId "attack"
                    ]
        
        learnedActions =
            learned
                |> Set.toList
                |> List.map Action.byId
    in
    baseActions ++ learnedActions

initActionStates : List Action -> List ActionState
initActionStates actions =
    actions
        |> List.map ActionState.initFromAction

completeBattle : SceneModel -> SceneModel
completeBattle m =
    { m
        | actionPoints = m.maxActionPoints
        , statusSet =
            m.statusSet
                |> StatusSet.completeBattle
        , actions = initActions m.equippedWeapon m.learned
        , actionStates = initActionStates <| initActions m.equippedWeapon m.learned
        , passives = initPassives m.learnedPassives
    }

initPassives : Set String -> List Passive
initPassives learnedPassives =
    learnedPassives
        |> Set.toList
        |> List.map Passive.byId