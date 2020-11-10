module Battle exposing
    ( Battle
    , State(..)
    , Actor(..)
    , new
    , completeRound
    , chooseMonsterAction
    , runPlayerAction
    , runMonsterAction
    )

import Random

import Distribution exposing (Distribution)
import NonEmptyList exposing (NonEmptyList)
import Util

import Action exposing (Action)
import Battler exposing (Battler)
import Behavior exposing (Behavior)
import Monster exposing (Monster)
import Formula exposing (Formula)
import Target exposing (Target)
import Status exposing (Status)
import Duration exposing (Duration)
import SceneModel exposing (SceneModel)

type alias Battle =
    { round : Int
    , monster : Monster
    , state : State
    }

type State
    = Ongoing
    | Done

type Actor
    = Player
    | Monster

foldActor : a -> a -> Actor -> a
foldActor player monster actor =
    case actor of
        Player ->
            player
        
        Monster ->
            monster

new : Monster -> Battle
new monster =
    { round = 1
    , monster = monster
    , state = Ongoing
    }

completeRound : ( Battle, SceneModel ) -> ( Battle, SceneModel )
completeRound ( b, s ) =
    let
        newBattle =
            { b
                | round = b.round + 1
                , monster =
                    Battler.completeRound b.monster
            }
        
        newSceneModel =
            Battler.completeRound s
    in
    ( newBattle, newSceneModel )
    


chooseMonsterAction : Battle -> Random.Generator Action
chooseMonsterAction battle =
    case battle.monster.behavior of
        Behavior.None ->
            Random.constant <| Action.byId "nothing"
        
        Behavior.Dummy ->
            Random.constant <| Action.byId "nothing"
        
        Behavior.Gremlin ->
            Random.weighted
                ( 50, Action.byId "nothing" )
                [ ( 50, Action.byId "attack" )
                ]
        
        Behavior.BunBun ->
            Random.weighted
                ( 100, Action.byId "attack" )
                []
        
        Behavior.Wyvern ->
            if battle.round == 1 then
                Random.constant <| Action.byId "mega-flare"
            else
                Random.constant <| Action.byId "nothing"

runPlayerAction : Action -> ( Battle, SceneModel ) -> ( Battle, SceneModel )
runPlayerAction action ( battle, player ) =
    let
        newPlayer =
            { player
                | actionPoints = player.actionPoints - action.actionPointCost
                , magicPoints = player.magicPoints - action.magicPointCost
            }
        
        ( newPlayer2, newMonster, newState ) =
            ( newPlayer, battle.monster, battle.state )
                |> Util.forEach action.formulas applyFormula
        
        newBattle =
            { battle
                | monster = newMonster
                , state = newState
            }
    in
    ( newBattle, newPlayer2 )

runMonsterAction : Action -> ( Battle, SceneModel ) -> ( Battle, SceneModel )
runMonsterAction action ( battle, player ) =
    let
        monster =
            battle.monster
        
        newMonster =
            { monster
                | actionPoints = monster.actionPoints - action.actionPointCost
                , magicPoints = monster.magicPoints - action.magicPointCost
            }
        
        ( newMonster2, newPlayer, newState ) =
            ( newMonster, player, battle.state )
                |> Util.forEach action.formulas applyFormula
        
        newBattle =
            { battle
                | monster = newMonster2
                , state = newState
            }
    in
    ( newBattle, newPlayer )

embedState : State -> ( Battler a, Battler b ) -> ( Battler a, Battler b, State )
embedState state ( a, b ) =
    ( a, b, state )

applyFormula : Formula -> ( Battler a, Battler b, State ) -> ( Battler a, Battler b, State )
applyFormula formula ( a, b, state ) =
    case formula of
        Formula.Attack ->
            ( a, b )
                |> Battler.takeDamage Target.Enemy (Battler.totalAttack a - b.block - Battler.totalDefense b)
                |> embedState state
        
        Formula.AxeAttack ->
            ( a, b )
                |> Battler.takeDamage Target.Enemy (3 * (Battler.totalAttack a) - b.block - Battler.totalDefense b)
                |> embedState state
        
        Formula.BowAttack ->
            ( a, b )
                |> Battler.takeDamage Target.Enemy (Battler.totalAttack a - b.block - Battler.totalDefense b)
                |> embedState state
        
        Formula.ClawAttack ->
            ( a, b )
                |> Battler.takeDamage Target.Enemy (Battler.totalAttack a - b.block - Battler.totalDefense b)
                |> embedState state
        
        Formula.StaffAttack ->
            ( a, b )
                |> Battler.takeDamage Target.Enemy (Battler.totalAttack a - b.block - Battler.totalDefense b)
                |> embedState state
        
        Formula.Block ->
            ( a, b )
                |> Battler.gainBlock Target.Self (Battler.totalVitality a)
                |> embedState state

        Formula.MegaFlare ->
            ( a, b )
                |> Battler.takeDamage Target.Enemy (2 * Battler.totalMagic a)
                |> embedState state
        
        Formula.ChargeUp i ->
            ( a, b )
                |> Battler.applyStatus Target.Self Status.ModifyAttack Duration.Battle i
                |> embedState state
        
        Formula.Explode ->
            ( a, b )
                |> Battler.takeDamage Target.Self a.hitPoints
                |> Battler.takeDamage Target.Enemy a.hitPoints
                |> embedState state
        
        Formula.Curse ->
            ( a, b )
                |> Battler.applyStatus Target.Enemy Status.Curse Duration.Persistent 1
                |> embedState state
        
        Formula.Poison ->
            ( a, b )
                |> Battler.applyStatus Target.Enemy Status.Poison Duration.Persistent 1
                |> embedState state
        
        Formula.HalfFire ->
            ( a, b )
                |> Battler.takeDamage Target.Enemy (2 * Battler.totalMagic a - Battler.totalMagicDefense b)
                |> Battler.applyStatus Target.Enemy Status.Burn (Duration.Rounds <| Battler.totalMagic a) 1
                |> embedState state
        
        Formula.Flee ->
            ( a, b, Done )