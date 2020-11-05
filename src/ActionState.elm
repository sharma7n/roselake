module ActionState exposing
    ( ActionState
    , State(..)
    , perform
    , tick
    , initFromAction
    , stateToString
    , canUse
    , performOneAction
    )

import Action exposing (Action)

import Util

type alias ActionState =
    { action : Action
    , state : State
    }

type State
    = Available
    | Cooldown Int

perform : ActionState -> ActionState
perform s =
    { s | state = Cooldown s.action.cooldown }

tick : ActionState -> ActionState
tick s =
    let
        newState =
            case s.state of
                Available ->
                    Available
                
                Cooldown amount ->
                    let
                        newAmount =
                            amount - 1
                    in
                    if newAmount <= 0 then
                        Available
                    else
                        Cooldown newAmount

    in
    { s | state = newState }

initFromAction : Action -> ActionState
initFromAction a =
    { action = a
    , state = Available
    }

stateToString : State -> String
stateToString s =
    case s of
        Available ->
            "Available"
        
        Cooldown i ->
            "Cooldown: " ++ String.fromInt i

canUse : Int -> ActionState -> Bool
canUse remainingActionPoints s =
    case s.state of
        Available ->
            s.action.actionPointCost <= remainingActionPoints
        
        _ ->
            False

performOneAction : Action -> List ActionState -> List ActionState
performOneAction action actionStates =
    Util.forEach actionStates (\head -> \tail ->
        if action.id == head.action.id then
            perform head :: tail
        else
            head :: tail
    ) []