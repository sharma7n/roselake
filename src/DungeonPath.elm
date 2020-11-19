module DungeonPath exposing
    ( Path
    , generator
    )

import Random

import Distribution exposing (Distribution)
import DungeonScene
import Requirement exposing (Requirement)
import Attribute exposing (Attribute)

type alias Path =
    { description : String
    , requirements : List Requirement
    , sceneDistribution : Distribution DungeonScene.Scene
    }

generator : Random.Generator Path
generator =
    let
        restArea =
            { description = "Save Point"
            , requirements =
                []
            , sceneDistribution =
                Distribution.new
                    ( 95, DungeonScene.RestArea )
                    [ ( 5, DungeonScene.TrapDoor )
                    ]
            }
        
        windingTunnels =
            { description = "Grab Bag"
            , requirements =
                []
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
            , requirements =
                []
            , sceneDistribution =
                Distribution.new
                    ( 100, DungeonScene.Empty )
                    []
            }
        
        monsterHunt =
            { description = "Monster Hunt"
            , requirements =
                []
            , sceneDistribution = 
                Distribution.new
                    ( 80, DungeonScene.Battle )
                    [ ( 20, DungeonScene.Treasure )
                    ]
            }
        
        monsterStealth =
            { description = "Monster Stealth"
            , requirements =
                []
            , sceneDistribution =
                Distribution.new
                    ( 60, DungeonScene.Empty )
                    [ ( 40, DungeonScene.Battle )
                    ]
            }
        
        ricketyBridge =
            { description = "Rickety Bridge"
            , requirements =
                []
            , sceneDistribution =
                Distribution.new
                    ( 50, DungeonScene.Empty )
                    [ ( 50, DungeonScene.TrapDoor )
                    ]
            }
        
        boulders =
            { description = "Boulders"
            , requirements =
                [ Requirement.AttributeRequirement Attribute.Strength 6
                ]
            , sceneDistribution =
                Distribution.new
                    ( 100, DungeonScene.Empty )
                    []
            }
    in
    Random.weighted
        ( 5, restArea )
        [ ( 30, windingTunnels )
        , ( 20, monsterHunt )
        , ( 20, monsterStealth )
        , ( 5, terrainPatch )
        , ( 10, ricketyBridge )
        , ( 1000, boulders )
        ]