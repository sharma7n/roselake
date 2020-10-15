module Target exposing
    ( Target(..)
    , toString
    )

type Target
    = None
    | Self
    | Enemy

toString : Target -> String
toString t =
    case t of
        None ->
            "None"
        
        Self ->
            "Self"
        
        Enemy ->
            "Enemy"