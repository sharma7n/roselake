module Phase exposing
    ( Phase(..)
    )

import CharacterCreationModel exposing (CharacterCreationModel)

import Scene exposing (Scene)
import Character exposing (Character)

type Phase
    = CharacterCreationPhase CharacterCreationModel
    | ScenePhase Scene Character