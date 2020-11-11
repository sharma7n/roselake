module Attribute exposing
    ( Attribute(..)
    , toString
    , toShortString
    )

type Attribute
    = Strength
    | Vitality
    | Agility
    | Intellect
    | Charisma

fold : a -> a -> a -> a -> a -> Attribute -> a
fold strength vitality agility intellect charisma a =
    case a of
        Strength ->
            strength
        
        Vitality ->
            vitality
        
        Agility ->
            agility
        
        Intellect ->
            intellect
        
        Charisma ->
            charisma

toString : Attribute -> String
toString =
    fold "Strength" "Vitality" "Agility" "Intellect" "Charisma"

toShortString : Attribute -> String
toShortString =
    fold "STR" "VIT" "AGI" "INT" "CHA"