module Battler exposing
    ( Battler
    , applyEffects
    )

import Util

import Effect exposing (Effect)
import Formula exposing (Formula)

import Weapon exposing (Weapon)

type alias Battler a =
    { a
        | hitPoints : Int
        , maxHitPoints : Int
        , magicPoints : Int
        , maxMagicPoints : Int
        , attack : Int
        , agility : Int
        , equippedWeapon : Maybe Weapon
    }

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
applyFormula formula ( self, enemy ) =
    case formula of
        Formula.Attack ->
            let
                dmg =
                    self.attack + Maybe.withDefault 0 (Maybe.map .attack self.equippedWeapon)
                
                newEnemy =
                    { enemy | hitPoints = Util.boundedBy 0 enemy.maxHitPoints (enemy.hitPoints - dmg) }
            in
            ( self, newEnemy )
        
        Formula.Fireball ->
            let
                dmg =
                    3
                
                newEnemy =
                    { enemy | hitPoints = Util.boundedBy 0 enemy.maxHitPoints (enemy.hitPoints - dmg) }
            in
            ( self, newEnemy )
        
        Formula.Heal ->
            let
                d =
                    2
                
                newSelf =
                    { self | hitPoints = Util.boundedBy 0 self.maxHitPoints (self.hitPoints + d ) }
            in
            ( newSelf, enemy )