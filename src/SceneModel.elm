module SceneModel exposing
    ( SceneModel
    , characterCreationModelToSceneModel
    , applyEffectsToSceneModel
    , applyReward
    , completeBattle
    , satisfiesRequirements
    )

import Set exposing (Set)

import FormResult
import Util

import CharacterCreationError
import CharacterCreationModel exposing (CharacterCreationModel)
import CharacterCreationSettings exposing (CharacterCreationSettings)
import HairStyle exposing (HairStyle)
import HairColor exposing (HairColor)
import EyeColor exposing (EyeColor)
import Complexion exposing (Complexion)
import Height exposing (Height)
import Build exposing (Build)


import Attribute exposing (Attribute)
import Action exposing (Action)
import Passive exposing (Passive)
import ActionState exposing (ActionState)
import Armor exposing (Armor)
import Avatar exposing (Avatar)
import Effect exposing (Effect)
import Essentia exposing (Essentia)
import Inventory exposing (Inventory)
import Reward exposing (Reward)
import Requirement exposing (Requirement)
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
    , hitPoints : Int
    , maxHitPoints : Int
    , magicPoints : Int
    , maxMagicPoints : Int
    , actionPoints : Int
    , maxActionPoints : Int
    , vitality : Int
    , strength : Int
    , defense : Int
    , magicDefense : Int
    , agility : Int
    , intellect : Int
    , actions : List Action
    , passives : List Passive
    , equippedWeapon : Maybe Weapon
    , equippedArmor : Maybe Armor
    , essentiaContainer : EssentiaContainer
    , statusSet : StatusSet
    , actionStates : List ActionState
    , block : Int
    }

getAttribute : Attribute -> SceneModel -> Int
getAttribute attr m =
    case attr of
        Attribute.Strength ->
            m.strength
        
        Attribute.Vitality ->
            m.vitality
        
        Attribute.Agility ->
            m.agility
        
        Attribute.Intellect ->
            m.intellect

applyEffectToSceneModel : Effect -> SceneModel -> SceneModel
applyEffectToSceneModel effect m =
    case effect of
        Effect.ChangeLevel d ->
            { m | level = max 1 (m.level + d) }
        
        Effect.ChangeExperience d ->
            { m | experience = max 0 (m.experience + d) }
        
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
            { m | strength = max 0 (m.strength + d) }
        
        _ ->
            m

applyEffectsToSceneModel : List Effect -> SceneModel -> SceneModel
applyEffectsToSceneModel effects m =
    List.foldl applyEffectToSceneModel m effects

characterCreationModelToSceneModel : CharacterCreationModel -> Result (List CharacterCreationError.Error) SceneModel
characterCreationModelToSceneModel model =
    FormResult.toValidation model.settings.name
        |> Result.andThen (\name -> FormResult.toValidation model.settings.hairStyle
        |> Result.andThen (\hairStyle -> FormResult.toValidation model.settings.hairColor
        |> Result.andThen (\hairColor -> FormResult.toValidation model.settings.eyeColor
        |> Result.andThen (\eyeColor -> FormResult.toValidation model.settings.complexion
        |> Result.andThen (\complexion -> FormResult.toValidation model.settings.height
        |> Result.andThen (\height -> FormResult.toValidation model.settings.build
        |> Result.andThen (\build -> FormResult.toValidation model.settings.startingWeapon
        |> Result.andThen (\startingWeapon -> FormResult.toValidation model.settings.startingEssentia
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
            , hitPoints = 10
            , maxHitPoints = 10
            , magicPoints = 5
            , maxMagicPoints = 5
            , actionPoints = 3
            , maxActionPoints = 3
            , vitality = model.vitality
            , strength = model.strength
            , defense = 0
            , magicDefense = 0
            , agility = model.agility
            , intellect = model.intellect
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

satisfiesRequirements : List Requirement -> SceneModel -> Bool
satisfiesRequirements rs m =
    rs
        |> List.all (\r ->
            satisfiesOneRequirement r m
        )

satisfiesOneRequirement : Requirement -> SceneModel -> Bool
satisfiesOneRequirement r m =
    case r of
        Requirement.AttributeRequirement attr i ->
            getAttribute attr m >= i