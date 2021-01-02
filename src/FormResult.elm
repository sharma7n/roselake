module FormResult exposing
    ( FormResult(..)
    , map
    , andThen
    , toValidation
    , toMaybe
    , fold
    , check
    )

type FormResult e a
    = FRBlank e
    | FROk a
    | FRErr e

map : (a -> b) -> (FormResult e a) -> (FormResult e b)
map f x =
    case x of
        FRBlank e ->
            FRBlank <| e
        
        FROk a ->
            FROk <| f a
        
        FRErr e ->
            FRErr <| e

andThen : (a -> FormResult e b) -> FormResult e a -> FormResult e b
andThen f x =
    case x of
        FRBlank e ->
            FRBlank e
        
        FROk a ->
            f a
        
        FRErr e ->
            FRErr e

toValidation : FormResult e a -> Result (List e) a
toValidation x =
    case x of
        FRBlank e ->
            Err <| List.singleton e
        
        FROk a ->
            Ok <| a
        
        FRErr e ->
            Err <| List.singleton e

fold : (e -> b) -> (a -> b) -> (e -> b) -> FormResult e a -> b
fold frBlankCase frOkCase frErrCase x =
    case x of
        FRBlank e ->
            frBlankCase e
        
        FROk a ->
            frOkCase a
        
        FRErr e ->
            frErrCase e

check : FormResult e a -> FormResult e a
check x =
    case x of
        FRBlank e ->
            FRErr e
        
        _ ->
            x

toMaybe : FormResult e a -> Maybe a
toMaybe =
    fold (\e -> Nothing) (\a -> Just a) (\e -> Nothing)