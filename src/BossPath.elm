module BossPath exposing
    ( BossPath
    , generator
    )

import Random

import Distribution exposing (Distribution)
import BossScene exposing (BossScene)
import BossBehavior exposing (BossBehavior)

type alias BossPath =
    { description : String
    , sceneDistribution : Distribution BossScene
    }

generator : BossBehavior -> Random.Generator BossPath
generator bossBehavior =
    case bossBehavior of
        BossBehavior.None ->
            Random.constant <|
                { description = "None"
                , sceneDistribution =
                    Distribution.new
                        ( 100, BossScene.Empty )
                        []
                }
        
        BossBehavior.Leviathan ->
            let
                iceBlock =
                    { description = "Leap onto ice block"
                    , sceneDistribution =
                        Distribution.new
                            ( 100, BossScene.Empty )
                            []
                    }
                
                fightBoss =
                    { description = "Fight boss"
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