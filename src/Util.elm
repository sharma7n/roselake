module Util exposing
    ( boundedBy
    , uncurry
    , forEach
    )

boundedBy : Int -> Int -> Int -> Int
boundedBy lower upper x =
    x
        |> min upper
        |> max lower

uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f (a, b)=
    f a b

forEach : List a -> (a -> b -> b) -> b -> b
forEach l f x =
    List.foldl f x l