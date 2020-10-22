module Battler exposing
    ( Battler
    , runAction
    , completeRound
    )

import Util

import Action exposing (Action)
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
        + List.foldl (\s -> \n ->
            case s of
                Status.ModifyAttack i ->
                    n + i
                
                _ ->
                    n
        ) 0 b.statuses

totalDefense : Battler a -> Int
totalDefense b =
    b.defense 
        + Debug.log "armordef" (Maybe.withDefault 0 (Maybe.map .defense b.equippedArmor))
        + List.foldl (\s -> \n ->
            case s of
                Status.ModifyDefense i ->
                    n + i
                
                _ ->
                    n
        ) 0 b.statuses

totalMagic : Battler a -> Int
totalMagic b =
    b.magic + Maybe.withDefault 0 (Maybe.map .magic b.equippedWeapon)

recoverhitPoints : Int -> Battler a -> Battler a
recoverhitPoints amt b =
    { b | hitPoints = Util.boundedBy 0 b.maxHitPoints (b.hitPoints + amt) }

takeDamage : Int -> Battler a -> Battler a
takeDamage dmg b =
    let
        receivedDamage =
            max 0 dmg
    in  
    { b 
        | hitPoints = Util.boundedBy 0 b.maxHitPoints (b.hitPoints - receivedDamage)
        , block = max 0 (b.block - receivedDamage)
    }

gainBlock : Int -> Battler a -> Battler a
gainBlock d b =
    Debug.log "new" { b | block = max 0 (b.block + d) }

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

completeRound : Battler a -> Battler a
completeRound b =
    { b
        | block = 0
    }

applyStatus : Status -> Battler a -> Battler a
applyStatus status b =
    case status of
        Status.ModifyAttack i ->
            let
                ( filtered, existingMod ) =
                    b.statuses
                        |> List.foldl (\s -> \(f, e) ->
                            case s of
                                Status.ModifyAttack j ->
                                    (f, e + j)
                                
                                _ ->
                                    (s :: f, e)
                        ) ([], 0)
                
                newStatus =
                    Status.ModifyAttack <| existingMod + i

            in
            { b | statuses = newStatus :: filtered }
        
        Status.ModifyDefense i ->
            let
                ( filtered, existingMod ) =
                    b.statuses
                        |> List.foldl (\s -> \(f, e) ->
                            case s of
                                Status.ModifyDefense j ->
                                    (f, e + j)
                                
                                _ ->
                                    (s :: f, e)
                        ) ([], 0)
                
                newStatus =
                    Status.ModifyDefense <| existingMod + i

            in
            { b | statuses = newStatus :: filtered }
            
applyFormula : Formula -> ( Battler a, Battler b ) -> ( Battler a, Battler b )
applyFormula formula ( a, b ) =
    case formula of
        Formula.Attack ->
            ( a 
            , b
                |> takeDamage (totalAttack a - b.block)
            )
        
        Formula.Block ->
            ( a
                |> gainBlock (Debug.log "totalDef" (totalDefense a))
            , b
            )

        Formula.FireBreath ->
            ( a 
            , b
                |> takeDamage (3 * totalMagic a)
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
                |> applyStatus (Status.ModifyAttack i)
            , b
            )
        
        Formula.Explode ->
            ( a
                |> takeDamage a.hitPoints
            , b
                |> takeDamage a.hitPoints
            )