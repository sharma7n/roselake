module Context exposing
    ( Context(..)
    , toString
    )

type Context
    = None
    | Any
    | Battle
    | Dungeon
    | Rest

toString : Context -> String
toString c =
    case c of
        None ->
            "None"
        
        Any ->
            "Any"
        
        Battle ->
            "Battle"
        
        Dungeon ->
            "Dungeon"
        
        Rest ->
            "Rest"