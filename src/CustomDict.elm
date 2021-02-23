module CustomDict exposing
  ( CustomDict
  , new
  , insert
  , get
  , member
  , keys
  , toList
  )

import Dict exposing (Dict)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)

type CustomDict a b
  = T (Dict String b)

aToString : (a -> Encode.Value) -> a -> String
aToString aEncoder a =
  Encode.encode 0 (aEncoder a)

aFromString : Decoder a -> String -> Maybe a
aFromString aDecoder s =
  Decode.decodeString aDecoder s
    |> Result.toMaybe

new : CustomDict a b
new =
  T <| Dict.empty

insert : (a -> Encode.Value) -> a -> b -> CustomDict a b -> CustomDict a b
insert aEncoder key val (T d) =
  d
    |> Dict.insert (aToString aEncoder key) val
    |> T

get : (a -> Encode.Value) -> a -> CustomDict a b -> Maybe b
get aEncoder key (T d) =
  d
    |> Dict.get (aToString aEncoder key)

keys : Decoder a -> CustomDict a b -> List a
keys aDecoder (T d) =
  d
    |> Dict.keys
    |> List.filterMap (aFromString aDecoder)

toList : Decoder a -> CustomDict a b -> List ( a, b )
toList aDecoder (T d) =
  d
    |> Dict.toList
    |> List.filterMap (\(k, v) ->
      case (aFromString aDecoder k) of
        Just a ->
          Just (a, v)
        
        Nothing ->
          Nothing
    )

member : (a -> Encode.Value) -> a -> CustomDict a b -> Bool
member aEncoder a (T d) =
  d
    |> Dict.member (aToString aEncoder a)