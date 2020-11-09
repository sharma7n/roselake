module Battler exposing
    ( Battler
    , completeRound
    , totalMaxHitPoints
    , takeDamage
    , gainBlock
    , applyStatus
    , totalAttack
    , totalDefense
    , totalMagic
    , totalMagicDefense
    , totalVitality
    )

import Util

import Action exposing (Action)
import Duration exposing (Duration)
import Effect exposing (Effect)
import Formula exposing (Formula)
import Target exposing (Target)

import Passive exposing (Passive)
import PassiveFormula exposing (PassiveFormula)

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
        , agility : Int
        , defense : Int
        , magicDefense : Int
        , equippedWeapon : Maybe Weapon
        , equippedArmor : Maybe Armor
        , statusSet : StatusSet
        , block : Int
        , passives : List Passive
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

totalMagicDefense : Battler a -> Int
totalMagicDefense b =
    b.magicDefense

totalVitality : Battler a -> Int
totalVitality b =
    b.vitality

totalMagic : Battler a -> Int
totalMagic b =
    b.magic + Maybe.withDefault 0 (Maybe.map .magic b.equippedWeapon)

totalMaxHitPoints : Battler a -> Int
totalMaxHitPoints b =
    max 1 (b.maxHitPoints + StatusSet.maxHitPoints b.statusSet)

recoverhitPoints : Target -> Int -> ( Battler a, Battler b ) -> ( Battler a, Battler b )
recoverhitPoints t amt ( a, b ) =
    let
        recoverOneHitPoints bb =
            { bb | hitPoints = Util.boundedBy 0 (totalMaxHitPoints bb) (bb.hitPoints + amt) }
    in
    case t of
        Target.Self ->
            ( recoverOneHitPoints a, b )
        
        Target.Enemy ->
            ( a, recoverOneHitPoints b )

takeDamage : Target -> Int -> ( Battler a, Battler b ) -> ( Battler a, Battler b )
takeDamage t dmg ( a, b ) =
    let
        receivedDamage =
            max 0 dmg
        
        takeOneDamage bb =
            { bb 
                | hitPoints = Util.boundedBy 0 (totalMaxHitPoints bb) (bb.hitPoints - receivedDamage)
                , block = max 0 (bb.block - receivedDamage)
            }
    in
    case t of
        Target.Self ->
            ( takeOneDamage a, b )
        
        Target.Enemy ->
            ( a, takeOneDamage b )
                |> Util.forEach b.passives (\passive -> \(x, y) ->
                    Util.forEach passive.effects (\passiveFormula -> \(xx, yy) ->
                        applyCounterFormula passiveFormula ( xx, yy )
                    ) ( x, y )
                )

applyCounterFormula : PassiveFormula -> ( Battler a, Battler b ) -> ( Battler a, Battler b )
applyCounterFormula passiveFormula ( a, b ) =
    case passiveFormula of
        PassiveFormula.PCounterDefend ->
            ( a, b )
                |> applyStatus Target.Enemy Status.ModifyBlock (Duration.Rounds 1) b.vitality
        
        PassiveFormula.PCounterTackle ->
            ( a, b )
                |> takeDamage Target.Self (totalAttack b - a.block)

gainBlock : Target -> Int -> ( Battler a, Battler b ) -> ( Battler a, Battler b )
gainBlock t d ( a, b ) =
    let
        gainOneBlock bb =
            { bb | block = max 0 (bb.block + d ) }
    in
    case t of
        Target.Self ->
            ( gainOneBlock a, b )
        
        Target.Enemy ->
            ( a, gainOneBlock b )

completeRound : Battler a -> Battler a
completeRound b =
    { b
        | block = StatusSet.block b.statusSet
        , statusSet =
            b.statusSet
                |> StatusSet.tick
        , hitPoints =
            Util.boundedBy 0 (totalMaxHitPoints b) (b.hitPoints - (StatusSet.hpLoss b.statusSet))
    }

applyStatus : Target -> Status -> Duration -> Int -> ( Battler a, Battler b ) -> ( Battler a, Battler b )
applyStatus t status duration stacks ( a, b ) =
    let
        applyOneStatus : Battler c -> Battler c
        applyOneStatus bb =
            let
                newStatusSet =
                    bb.statusSet
                        |> StatusSet.apply status duration stacks
            in
            { bb
                | statusSet = newStatusSet
            }
    
    in
    case t of
        Target.Self ->
            ( applyOneStatus a, b )
        
        Target.Enemy ->
            ( a, applyOneStatus b )