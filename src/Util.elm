module Util exposing
    ( boundedBy
    , uncurry
    , forEach
    , forCount
    , appendMaybe
    , removeListAt
    , getById
    , kebabify
    , uniformGenerator
    , betweenInclusive
    , betweenExclusive
    , randomDistinctList
    , maybeToList
    )

import Dict exposing (Dict)
import Random

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

forCount : Int -> (a -> a) -> a -> a
forCount n f x =
    List.foldl (\_ -> f) x (List.repeat n ())


appendMaybe : Maybe a -> List a -> List a
appendMaybe m l =
    case m of
        Just x ->
            x :: l
        
        Nothing ->
            l

removeListAt : Int -> List a -> List a
removeListAt idx l =
    l
        |> List.indexedMap (\i -> \x -> (i, x))
        |> List.filter(\(i,_) -> i /= idx)
        |> List.map (\(_, x) -> x)

getById : List { a | id : String } -> { a | id : String } -> String -> { a | id : String }
getById all default id =
    all
        |> List.filter (\a -> a.id == id)
        |> List.head
        |> Maybe.withDefault default

kebabify : String -> String
kebabify str =
    str
        |> String.split " "
        |> List.map String.toLower
        |> String.join "-"

uniformGenerator : a -> List a -> Random.Generator a
uniformGenerator ignored all =
    Random.weighted
        ( 0, ignored )
        ( List.map (\a -> ( 1, a )) all)

betweenInclusive : Int -> Int -> Int -> Bool
betweenInclusive lower upper n =
    lower <= n && n <= upper

betweenExclusive : Int -> Int -> Int -> Bool
betweenExclusive lower upper n =
    lower < n && n < upper

randomDistinctList : Int -> Random.Generator { a | id : String } -> Random.Generator (List { a | id : String })
randomDistinctList n gen =
    let
        loop iters rState =
            rState
                |> Random.andThen (\state ->
                    if iters < 128 && Dict.size state < n then
                        gen
                            |> Random.andThen (\got ->
                                let
                                    newRState =
                                        state
                                            |> Dict.insert got.id got
                                            |> Random.constant
                                in
                                loop (iters + 1) newRState
                            )
                    else
                        Random.constant state
                )
    in
    Random.constant Dict.empty
        |> loop 0
        |> Random.map Dict.values

maybeToList : Maybe a -> List a
maybeToList m =
    case m of
        Just a ->
            [a]
        
        Nothing ->
            []