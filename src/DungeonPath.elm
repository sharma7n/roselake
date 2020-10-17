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
        windingTunnels =
            { description = "Follow the winding tunnels"
            , sceneDistribution =
                Distribution.new
                    ( 40, DungeonScene.Empty )
                    [ ( 40, DungeonScene.Battle )
                    , ( 20, DungeonScene.Treasure )
                    ]
            }
    in
    Random.constant windingTunnels