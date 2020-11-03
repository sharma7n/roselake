module ActionState exposing
    ( ActionState
    , State(..)
    , perform
    , tick
    , initFromAction
    )

import Action exposing (Action)

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