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
                    ( 100, DungeonScene.Empty )
                    []
            }
    in
    Random.constant windingTunnels