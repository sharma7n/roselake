module SceneState exposing
    ( SceneState(..)
    )

import Battle exposing (Battle)
import Delve exposing (Delve)

type SceneState
    = Normal
    | Delving Delve
    | Battling Battle