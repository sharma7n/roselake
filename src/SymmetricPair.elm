module SymmetricPair exposing
  ( SymmetricPair
  , encoder
  , decoder
  , toPair
  , toString
  , fromString
  , contains
  )

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)

type SymmetricPair a
  = T a a

order : (a -> Encode.Value) -> SymmetricPair a -> b -> b -> b
order aEncoder (T x y) l r =
  if aToString aEncoder x <= aToString aEncoder y then l else r

aToString : (a -> Encode.Value) -> a -> String
aToString aEncoder a =
  Encode.encode 0 (aEncoder a)

encoder : (a -> Encode.Value) -> SymmetricPair a -> Encode.Value
encoder aEncoder (T x y) =
  let
    encodePair z w =
      Encode.object
        [ ( "first", aEncoder z )
        , ( "second", aEncoder w )
        ]
  in
  order aEncoder (T x y)
    (encodePair x y)
    (encodePair y x)

decoder : Decoder a -> Decoder (SymmetricPair a)
decoder aDecoder =
  Decode.map2 T
    (Decode.field "first" aDecoder)
    (Decode.field "second" aDecoder)

toPair : (a -> Encode.Value) -> SymmetricPair a -> ( a, a )
toPair aEncoder (T x y) =
  order aEncoder (T x y) 
    ( x, y ) 
    ( y, x )

toString : (a -> Encode.Value) -> SymmetricPair a -> String
toString aEncoder (T x y) =
  Encode.encode 0 (encoder aEncoder (T x y))

fromString : Decoder a -> String -> Maybe (SymmetricPair a)
fromString aDecoder s =
  s
    |> Decode.decodeString (decoder aDecoder)
    |> Result.toMaybe

contains : (a -> Encode.Value) -> a -> SymmetricPair a -> Bool
contains aEncoder a (T x y) =
  aToString aEncoder a == aToString aEncoder x || aToString aEncoder a == aToString aEncoder y