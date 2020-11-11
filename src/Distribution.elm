module Distribution exposing
    ( Distribution
    , new
    , uniform
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

uniform : a -> List a -> Distribution a
uniform head tail =
    Distribution <| NonEmptyList.new ( 1, head ) ( List.map (\t -> ( 1, t )) tail )

random : Distribution a -> Random.Generator a
random (Distribution l) =
    Random.weighted (NonEmptyList.head l) (NonEmptyList.tail l)

toList : Distribution a -> List ( Float, a )
toList (Distribution l) =
    NonEmptyList.toList l