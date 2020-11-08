module Target exposing
    ( Target(..)
    , toString
    )

type Target
    = Self
    | Enemy

toString : Target -> String
toString t =
    case t of
        Self ->
            "Self"
        
        Enemy ->
            "Enemy"