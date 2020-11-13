module BossPath exposing
    ( BossPath
    , generator
    )

import Random

import Distribution exposing (Distribution)
import BossScene exposing (BossScene)

type alias BossPath =
    { description : String
    , sceneDistribution : Distribution BossScene
    }

generator : Random.Generator BossPath
generator =
    Random.constant <|
        { description = "Leap onto ice block"
        , sceneDistribution =
            Distribution.new
                ( 1, BossScene.Empty )
                [ ( 1, BossScene.Empty )
                ]
        }