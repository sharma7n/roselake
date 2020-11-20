module BossPath exposing
    ( BossPath
    , generator
    )

import Random

import Distribution exposing (Distribution)
import BossScene exposing (BossScene)
import BossBehavior exposing (BossBehavior)

type alias BossPath =
    { id : String
    , description : String
    , sceneDistribution : Distribution BossScene
    }

generator : BossBehavior -> Random.Generator BossPath
generator bossBehavior =
    case bossBehavior of
        BossBehavior.None ->
            Random.constant <|
                { id = "none"
                , description = "None"
                , sceneDistribution =
                    Distribution.new
                        ( 100, BossScene.Empty )
                        []
                }
        
        BossBehavior.Leviathan ->
            let
                iceBlock =
                    { id = "terrain"
                    , description = "Terrain"
                    , sceneDistribution =
                        Distribution.new
                            ( 100, BossScene.Empty )
                            []
                    }
                
                fightBoss =
                    { id = "fight-boss"
                    , description = "Fight Boss"
                    , sceneDistribution =
                        Distribution.new
                            ( 100, BossScene.BattleBoss )
                            []
                    }
            in
            Distribution.random <| Distribution.new
                ( 50, iceBlock )
                [ ( 50, fightBoss )
                ]