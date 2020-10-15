module Util exposing
    ( boundedBy
    )

boundedBy : Int -> Int -> Int -> Int
boundedBy lower upper x =
    x
        |> min upper
        |> max lower