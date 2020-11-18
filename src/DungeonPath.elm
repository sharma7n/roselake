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
            { description = "Save Point"
            , sceneDistribution =
                Distribution.new
                    ( 95, DungeonScene.RestArea )
                    [ ( 5, DungeonScene.TrapDoor )
                    ]
            }
        
        windingTunnels =
            { description = "Grab Bag"
            , sceneDistribution =
                Distribution.new
                    ( 20, DungeonScene.Empty )
                    [ ( 35, DungeonScene.Battle )
                    , ( 10, DungeonScene.Treasure )
                    , ( 20, DungeonScene.RestArea )
                    , ( 5, DungeonScene.TrapDoor )
                    , ( 10, DungeonScene.Shop )
                    ]
            }
        
        terrainPatch =
            { description = "Terrain Patch"
            , sceneDistribution =
                Distribution.new
                    ( 100, DungeonScene.Empty )
                    []
            }
        
        monsterNest =
            { description = "Monster Nest"
            , sceneDistribution = 
                Distribution.new
                    ( 10, DungeonScene.Empty )
                    [ ( 70, DungeonScene.Battle )
                    , ( 20, DungeonScene.Treasure )
                    ]
            }
    in
    Random.weighted
        ( 5, restArea )
        [ ( 45, windingTunnels )
        , ( 45, monsterNest )
        , ( 5, terrainPatch )
        ]