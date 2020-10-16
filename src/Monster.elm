module Monster exposing
    ( Monster
    , byId
    , generator
    , chooseAction
    )

import Random

import Distribution exposing (Distribution)

import Action exposing (Action)

type alias Monster =
    { name : String
    , experience : Int
    , gold : Int
    , abilityPoints : Int
    , hitPoints : Int
    , maxHitPoints : Int
    , magicPoints : Int
    , maxMagicPoints : Int
    , attack : Int
    , agility : Int
    , actions : Distribution Action
    }

byId : String -> Monster
byId id =
    case id of
        "slime" ->
            { name = "Slime"
            , experience = 1
            , gold = 1
            , abilityPoints = 1
            , hitPoints = 3
            , maxHitPoints = 3
            , magicPoints = 0
            , maxMagicPoints = 0
            , attack = 1
            , agility = 1
            , actions =
                Distribution.new
                    ( 50, Action.byId "nothing" )
                    [ ( 50, Action.byId "attack" )
                    ]
            }
        
        "dummy" ->
            { name = "Dummy"
            , experience = 1
            , gold = 1
            , abilityPoints = 1
            , hitPoints = 1
            , maxHitPoints = 1
            , magicPoints = 1
            , maxMagicPoints = 1
            , attack = 1
            , agility = 1
            , actions = Distribution.new
                ( 1, Action.byId "nothing" )
                []
            }
        
        "gremlin" ->
            { name = "Gremlin"
            , experience = 1
            , gold = 1
            , abilityPoints = 1
            , hitPoints = 10
            , maxHitPoints = 10
            , magicPoints = 5
            , maxMagicPoints = 5
            , attack = 1
            , agility = 1
            , actions =
                Distribution.new
                    ( 33, Action.byId "attack" )
                    [ ( 33, Action.byId "fireball" )
                    , ( 33, Action.byId "heal" )
                    ]
            }
        
        _ ->
            { name = "Null Monster"
            , experience = 0
            , gold = 0
            , abilityPoints = 0
            , hitPoints = 0
            , maxHitPoints = 0
            , magicPoints = 0
            , maxMagicPoints = 0
            , attack = 0
            , agility = 0
            , actions =
                Distribution.new
                    ( 0, Action.byId "null" )
                    []
            }

generator : Random.Generator Monster
generator =
    Random.weighted
        ( 0, byId "" )
        [ ( 1, byId "slime" )
        ]

chooseAction : Monster -> Random.Generator Action
chooseAction monster =
    chooseActionInternal monster 0

chooseActionInternal : Monster -> Int -> Random.Generator Action
chooseActionInternal monster retries =
    if retries > 2 then
        Random.constant <| Action.byId "attack"
    else
        Distribution.random monster.actions
            |> Random.andThen (\action ->
                if action.magicPointCost <= monster.magicPoints then
                    Random.constant action
                else
                    chooseActionInternal monster (retries + 1)
            )