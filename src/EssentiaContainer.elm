module EssentiaContainer exposing
    ( EssentiaContainer
    , Index(..)
    , indexToString
    , listIndices
    , getSlot
    , setSlot
    , clearSlot
    , new
    , toList
    )

import Essentia exposing (Essentia)

type EssentiaContainer
    = EssentiaContainer Data

type Index
    = Index1
    | Index2

type alias Data =
    { index1 : Maybe Essentia
    , index2 : Maybe Essentia
    }

foldIndices : a -> a -> Index -> a
foldIndices val1 val2 i =
    case i of
        Index1 ->
            val1
        
        Index2 ->
            val2

listIndices : List Index
listIndices =
    [ Index1
    , Index2
    ]

indexToString : Index -> String
indexToString =
    foldIndices "1" "2"

getSlot : Index -> EssentiaContainer -> Maybe Essentia
getSlot i (EssentiaContainer data)=
    foldIndices data.index1 data.index2 i

setSlot : Index -> Essentia -> EssentiaContainer -> EssentiaContainer
setSlot i essentia (EssentiaContainer data) =
    EssentiaContainer <|
        case i of
            Index1 ->
                { data | index1 = Just essentia }
            
            Index2 ->
                { data | index2 = Just essentia }

clearSlot : Index -> EssentiaContainer -> EssentiaContainer
clearSlot i (EssentiaContainer data) =
    EssentiaContainer <|
        case i of
            Index1 ->
                { data | index1 = Nothing }
            
            Index2 ->
                { data | index2 = Nothing }

new : EssentiaContainer
new =
    EssentiaContainer <|
        { index1 = Nothing
        , index2 = Nothing
        }

toList : EssentiaContainer -> List (Index, Maybe Essentia)
toList (EssentiaContainer data) =
    [ ( Index1, data.index1 )
    , ( Index2, data.index2 )
    ]