module BossPath exposing
    ( Path
    , generator
    )

import Random

import Distribution exposing (Distribution)
import BossScene

type alias Path =
    { description : String
    , sceneDistribution : Distribution BossScene.Scene
    }

generator : Random.Generator Path
generator =
    Random.constant <|
        { description = "Leap onto ice block"
        , sceneDistribution =
            Distribution.new
                ( 1, BossScene.Empty )
                [ ( 1, BossScene.Empty )
                ]
        }