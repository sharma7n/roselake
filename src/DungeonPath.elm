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
                    ( 1, DungeonScene.Empty )
                    [ ( 1, DungeonScene.Battle )
                    ]
            }
    in
    Random.constant windingTunnels