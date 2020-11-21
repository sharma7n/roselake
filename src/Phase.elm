module Phase exposing
    ( Phase(..)
    )

import CharacterCreationModel exposing (CharacterCreationModel)

import Scene exposing (Scene)
import SceneState exposing (SceneState)
import Character exposing (Character)

type Phase
    = CharacterCreationPhase CharacterCreationModel
    | ScenePhase Scene SceneState Character