module StatusSet exposing
    ( StatusSet
    , empty
    , apply
    , tick
    , completeBattle
    )

import Dict exposing (Dict)

import Duration exposing (Duration)
import Status exposing (Status)

type StatusSet
    = StatusSet (Dict String Data)

type alias Data =
    { status : Status
    , duration : Duration
    , stacks : Int
    }

empty : StatusSet
empty =
    StatusSet Dict.empty

apply : Status -> Duration -> Int -> StatusSet -> StatusSet
apply status duration stacks (StatusSet dict) =
    let
        maybeData =
            dict
                |> Dict.get (Status.id status)
        
        newData =
            case maybeData of
                Just data ->
                    { status = status
                    , duration = Duration.add duration data.duration
                    , stacks = max 0 (stacks + data.stacks)
                    }

                Nothing ->
                    { status = status
                    , duration = duration
                    , stacks = stacks
                    }
        
        newDict =
            dict
                |> Dict.insert (Status.id status) newData
    in
    StatusSet newDict

tick : StatusSet -> StatusSet
tick (StatusSet dict) =
    let
        newDict =
            dict
                |> Dict.map (\_ -> \data ->
                    { data | duration = Duration.tick data.duration }
                )
                |> Dict.filter (\_ -> \data ->
                    not (Duration.expired data.duration)
                )
    in
    StatusSet newDict

completeBattle : StatusSet -> StatusSet
completeBattle (StatusSet dict) =
    let
        newDict =
            dict |>
                Dict.filter (\_ -> \data ->
                    data.duration == Duration.Persistent
                )
    in
    StatusSet newDict