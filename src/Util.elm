module Util exposing
    ( boundedBy
    , uncurry
    )

boundedBy : Int -> Int -> Int -> Int
boundedBy lower upper x =
    x
        |> min upper
        |> max lower

uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f (a, b)=
    f a b