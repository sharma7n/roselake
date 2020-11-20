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
    { id : String
    , description : String
    , requirements : List Requirement
    , sceneDistribution : Distribution DungeonScene.Scene
    }

generator : Random.Generator Path
generator =
    let
        restArea =
            { id = "save-point"
            , description = "Save Point"
            , requirements =
                []
            , sceneDistribution =
                Distribution.new
                    ( 95, DungeonScene.RestArea )
                    [ ( 5, DungeonScene.TrapDoor )
                    ]
            }
        
        terrainPatch =
            { id = "terrain-patch"
            , description = "Terrain Patch"
            , requirements =
                []
            , sceneDistribution =
                Distribution.new
                    ( 100, DungeonScene.Empty )
                    []
            }
        
        monsterHunt =
            { id = "monster-hunt"
            , description = "Monster Hunt"
            , requirements =
                []
            , sceneDistribution = 
                Distribution.new
                    ( 80, DungeonScene.Battle )
                    [ ( 20, DungeonScene.Treasure )
                    ]
            }
        
        monsterStealth =
            { id = "monster-stealth"
            , description = "Monster Stealth"
            , requirements =
                []
            , sceneDistribution =
                Distribution.new
                    ( 60, DungeonScene.Empty )
                    [ ( 40, DungeonScene.Battle )
                    ]
            }
        
        ricketyBridge =
            { id = "rickety-bridge"
            , description = "Rickety Bridge"
            , requirements =
                []
            , sceneDistribution =
                Distribution.new
                    ( 50, DungeonScene.Empty )
                    [ ( 50, DungeonScene.TrapDoor )
                    ]
            }
        
        boulders =
            { id = "boulders"
            , description = "Boulders"
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
        ( 10, restArea )
        [ ( 25, monsterHunt )
        , ( 25, monsterStealth )
        , ( 10, terrainPatch )
        , ( 15, ricketyBridge )
        , ( 15, boulders )
        ]