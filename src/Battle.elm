module Battle exposing
    ( Battle
    , completeRound
    )

import Battler exposing (Battler)
import Monster exposing (Monster)

type alias Battle =
    { round : Int
    , monster : Monster
    }

completeRound : Battle -> Battle
completeRound b =
    { round = b.round + 1
    , monster =
        Battler.completeRound b.monster
    }