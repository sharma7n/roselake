module Duration exposing
    ( Duration(..)
    , add
    , tick
    , expired
    )

type Duration
    = Rounds Int
    | Battle
    | Persistent

add : Duration -> Duration -> Duration
add a b =
    case ( a, b ) of
        ( Persistent, _ ) ->
            Persistent
        
        ( _, Persistent ) ->
            Persistent
        
        ( Battle, _ ) ->
            Battle
        
        ( _, Battle ) ->
            Battle
        
        ( Rounds x, Rounds y ) ->
            Rounds <| x + y

tick : Duration -> Duration
tick d =
    case d of
        Rounds n ->
            Rounds <| max 0 (n - 1)
        
        _ ->
            d

expired : Duration -> Bool
expired d =
    case d of
        Rounds n ->
            n <= 0
        
        _ ->
            False