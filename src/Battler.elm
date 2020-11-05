module Battler exposing
    ( Battler
    , runAction
    , completeRound
    , totalMaxHitPoints
    )

import Util

import Action exposing (Action)
import Duration exposing (Duration)
import Effect exposing (Effect)
import Formula exposing (Formula)

import Armor exposing (Armor)
import Status exposing (Status)
import Weapon exposing (Weapon)

import StatusSet exposing (StatusSet)

type alias Battler a =
    { a
        | hitPoints : Int
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
        , equippedWeapon : Maybe Weapon
        , equippedArmor : Maybe Armor
        , statusSet : StatusSet
        , block : Int
    }

totalAttack : Battler a -> Int
totalAttack b =
    b.attack 
        + Maybe.withDefault 0 (Maybe.map .attack b.equippedWeapon)
        + StatusSet.attack b.statusSet

totalDefense : Battler a -> Int
totalDefense b =
    b.defense 
        + Maybe.withDefault 0 (Maybe.map .defense b.equippedArmor)
        + StatusSet.defense b.statusSet

totalVitality : Battler a -> Int
totalVitality b =
    b.vitality

totalMagic : Battler a -> Int
totalMagic b =
    b.magic + Maybe.withDefault 0 (Maybe.map .magic b.equippedWeapon)

totalMaxHitPoints : Battler a -> Int
totalMaxHitPoints b =
    max 1 (b.maxHitPoints + StatusSet.maxHitPoints b.statusSet)

recoverhitPoints : Int -> Battler a -> Battler a
recoverhitPoints amt b =
    { b | hitPoints = Util.boundedBy 0 (totalMaxHitPoints b) (b.hitPoints + amt) }

takeDamage : Int -> Battler a -> Battler a
takeDamage dmg b =
    let
        receivedDamage =
            max 0 dmg
    in  
    { b 
        | hitPoints = Util.boundedBy 0 (totalMaxHitPoints b) (b.hitPoints - receivedDamage)
        , block = max 0 (b.block - receivedDamage)
    }

gainBlock : Int -> Battler a -> Battler a
gainBlock d b =
    { b | block = max 0 (b.block + d) }

runAction : Action -> ( Battler a, Battler b ) -> ( Battler a, Battler b )
runAction action ( attacker, defender ) =
    let
        effects =
            action.formulas
                |> List.map Effect.BattleFormula
        
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

completeRound : Battler a -> Battler a
completeRound b =
    { b
        | block = 0
        , statusSet =
            b.statusSet
                |> StatusSet.tick
        , hitPoints =
            Util.boundedBy 0 (totalMaxHitPoints b) (b.hitPoints - (StatusSet.hpLoss b.statusSet))
    }

applyStatus : Status -> Duration -> Int -> Battler a -> Battler a
applyStatus status duration stacks b =
    { b | statusSet =
        b.statusSet
            |> StatusSet.apply status duration stacks
    }

applyFormula : Formula -> ( Battler a, Battler b ) -> ( Battler a, Battler b )
applyFormula formula ( a, b ) =
    case formula of
        Formula.Attack ->
            ( a 
            , b
                |> takeDamage (totalAttack a - b.block)
            )
        
        Formula.AxeAttack ->
            ( a
            , b
                |> takeDamage (3 * (totalAttack a) - b.block)
            )
        
        Formula.BowAttack ->
            ( a
            , b
                |> takeDamage (totalAttack a - b.block)
            )
        
        Formula.ClawAttack ->
            ( a
            , b
                |> takeDamage (totalAttack a - b.block)
            )
        
        Formula.StaffAttack ->
            ( a
            , b
                |> takeDamage (totalAttack a - b.block)
            )
        
        Formula.Block ->
            ( a
                |> gainBlock (totalVitality a)
            , b
            )

        Formula.MegaFlare ->
            ( a 
            , b
                |> takeDamage (2 * totalMagic a)
            )
        
        Formula.Fire level ->
            let
                multiplier =
                    1 + (2 * level)  
            in
            ( a 
            , b
                |> takeDamage (multiplier * totalMagic a)
            )
        
        Formula.Heal level ->
            let
                multiplier =
                    1 + (2 * level)
            in
            ( a
                |> recoverhitPoints (multiplier * totalMagic a)
            , b
            )
        
        Formula.ChargeUp i ->
            ( a
                |> applyStatus Status.ModifyAttack Duration.Battle i
            , b
            )
        
        Formula.Explode ->
            ( a
                |> takeDamage a.hitPoints
            , b
                |> takeDamage a.hitPoints
            )
        
        Formula.Curse ->
            ( a
            , b
                |> applyStatus Status.Curse Duration.Persistent 1
            )
        
        Formula.Poison ->
            ( a
            , b
                |> applyStatus Status.Poison Duration.Persistent 1
            )