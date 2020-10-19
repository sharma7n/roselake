module Battler exposing
    ( Battler
    , runAction
    )

import Util

import Action exposing (Action)
import Effect exposing (Effect)
import Formula exposing (Formula)

import Armor exposing (Armor)
import Status exposing (Status)
import Weapon exposing (Weapon)

type alias Battler a =
    { a
        | hitPoints : Int
        , maxHitPoints : Int
        , magicPoints : Int
        , maxMagicPoints : Int
        , actionPoints : Int
        , maxActionPoints : Int
        , attack : Int
        , magic : Int
        , defense : Int
        , agility : Int
        , equippedWeapon : Maybe Weapon
        , equippedArmor : Maybe Armor
        , statuses : List Status
    }

totalAttack : Battler a -> Int
totalAttack b =
    b.attack + Maybe.withDefault 0 (Maybe.map .attack b.equippedWeapon)

totalDefense : Battler a -> Int
totalDefense b =
    b.defense + Maybe.withDefault 0 (Maybe.map .defense b.equippedArmor)

totalMagic : Battler a -> Int
totalMagic b =
    b.magic + Maybe.withDefault 0 (Maybe.map .magic b.equippedWeapon)

recoverhitPoints : Int -> Battler a -> Battler a
recoverhitPoints amt b =
    { b | hitPoints = Util.boundedBy 0 b.maxHitPoints (b.hitPoints + amt) }

takeDamage : Int -> Battler a -> Battler a
takeDamage dmg b =
    { b | hitPoints = Util.boundedBy 0 b.maxHitPoints (b.hitPoints - dmg) }

runAction : Action -> ( Battler a, Battler b ) -> ( Battler a, Battler b )
runAction action ( attacker, defender ) =
    let
        effects =
            action.subs
                |> List.map .effects
                |> List.concat
        
        newAttacker =
            { attacker
                | actionPoints = attacker.actionPoints - action.actionPointCost
                , magicPoints = attacker.magicPoints - action.magicPointCost
            }
    in
    ( newAttacker, defender )
        |> applyEffects effects

applyEffect : Effect -> ( Battler a, Battler b ) -> ( Battler a, Battler b )
applyEffect effect ( self, enemy ) =
    case effect of
        Effect.BattleFormula formula ->
            applyFormula formula ( self, enemy )
        
        _ ->
            ( self, enemy )

applyEffects : List Effect -> ( Battler a, Battler b ) -> (Battler a, Battler b )
applyEffects effects battlers =
    List.foldl applyEffect battlers effects

applyFormula : Formula -> ( Battler a, Battler b ) -> ( Battler a, Battler b )
applyFormula formula ( a, b ) =
    case formula of
        Formula.Attack ->
            ( a 
            , b
                |> takeDamage (totalAttack a - totalDefense b)
            )
        
        Formula.Fireball ->
            ( a 
            , b
                |> takeDamage (3 * totalMagic a)
            )
        
        Formula.Heal ->
            ( a
                |> recoverhitPoints (2 * totalMagic a)
            , b
            )