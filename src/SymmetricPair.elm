module SymmetricPair exposing
  ( SymmetricPair
  , toPair
  , toString
  , distinctPairs
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

distinctPairs : (a -> String) -> List a -> List (SymmetricPair a)
distinctPairs aToString la =
  []