module Distribution exposing
    ( Distribution
    , new
    , random
    )

import Random

type alias Distribution a =
    { head : ( Float, a )
    , tail : List ( Float, a )
    }

new : ( Float, a ) -> List ( Float, a ) -> Distribution a
new head tail =
    { head = head
    , tail = tail
    }

random : Distribution a -> Random.Generator a
random d =
    Random.weighted d.head d.tail