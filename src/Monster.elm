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
                    ( 50, Action.byId "attack" )
                    [ ( 50, Action.byId "fireball" )
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
    Distribution.random monster.actions