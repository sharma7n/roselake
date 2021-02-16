module SymmetricPair exposing
  ( SymmetricPair
  , toPair
  , toString
  , contains
  )

type SymmetricPair a
  = T a a

order : (a -> String) -> SymmetricPair a -> b -> b -> b
order aToString (T x y) l r =
  if aToString x <= aToString y then l else r

toPair : (a -> String) -> SymmetricPair a -> ( a, a )
toPair aToString (T x y) =
  order aToString (T x y) 
    ( x, y ) 
    ( y, x )

toString : (a -> String) -> SymmetricPair a -> String
toString aToString (T x y) =
  order aToString (T x y) 
    (aToString x ++ "|" ++ aToString y)
    (aToString y ++ "|" ++ aToString x)

contains : (a -> String) -> a -> SymmetricPair a -> Bool
contains aToString a (T x y) =
  aToString a == aToString x || aToString a == aToString y