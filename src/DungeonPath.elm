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
        
        monsterHunt =
            { description = "Monster Hunt"
            , sceneDistribution = 
                Distribution.new
                    ( 80, DungeonScene.Battle )
                    [ ( 20, DungeonScene.Treasure )
                    ]
            }
        
        monsterStealth =
            { description = "Monster Stealth"
            , sceneDistribution =
                Distribution.new
                    ( 60, DungeonScene.Empty )
                    [ ( 40, DungeonScene.Battle )
                    ]
            }
        
        ricketyBridge =
            { description = "Rickety Bridge"
            , sceneDistribution =
                Distribution.new
                    ( 50, DungeonScene.Empty )
                    [ ( 50, DungeonScene.TrapDoor )
                    ]
            }
    in
    Random.weighted
        ( 5, restArea )
        [ ( 40, windingTunnels )
        , ( 20, monsterHunt )
        , ( 20, monsterStealth )
        , ( 5, terrainPatch )
        , ( 10, ricketyBridge )
        ]