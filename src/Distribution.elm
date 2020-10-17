module Distribution exposing
    ( Distribution
    , new
    , random
    , toList
    )

import Random

import NonEmptyList exposing (NonEmptyList)

type Distribution a
    = Distribution (NonEmptyList ( Float, a ))

new : ( Float, a ) -> List ( Float, a ) -> Distribution a
new head tail =
    Distribution <| NonEmptyList.new head tail

random : Distribution a -> Random.Generator a
random (Distribution l) =
    Random.weighted (NonEmptyList.head l) (NonEmptyList.tail l)

toList : Distribution a -> List ( Float, a )
toList (Distribution l) =
    NonEmptyList.toList l