module BossState exposing
    ( BossState
    )

import Boss exposing (Boss)
import Monster exposing (Monster)

type alias BossState =
    { boss : Boss
    , monster : Monster
    }