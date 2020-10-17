module DungeonPath exposing
    ( Path
    , generator
    )

import Random

import Distribution exposing (Distribution)
import DungeonScene

type alias Path =
    { description : String
    , sceneDistribution : Distribution DungeonScene.Scene
    }

generator : Random.Generator Path
generator =
    let
        restArea =
            { description = "The glow of a save point"
            , sceneDistribution =
                Distribution.new
                    ( 100, DungeonScene.RestArea )
                    []
            }
        
        windingTunnels =
            { description = "Follow the winding tunnels"
            , sceneDistribution =
                Distribution.new
                    ( 15, DungeonScene.Empty )
                    [ ( 35, DungeonScene.Battle )
                    , ( 20, DungeonScene.Treasure )
                    , ( 25, DungeonScene.RestArea )
                    , ( 5, DungeonScene.TrapDoor )
                    ]
            }
    in
    Random.weighted
        ( 20, restArea )
        [ ( 80, windingTunnels )
        ]