module Phase exposing
    ( Phase(..)
    )

import CharacterCreationModel exposing (CharacterCreationModel)

import Scene exposing (Scene)
import SceneModel exposing (SceneModel)

type Phase
    = CharacterCreationPhase CharacterCreationModel
    | ScenePhase Scene SceneModel