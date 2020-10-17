module NonEmptyList exposing
    ( NonEmptyList
    , new
    , toList
    , map
    , head
    , tail
    )

type NonEmptyList a
    = NonEmptyList a (List a)

new : a -> List a -> NonEmptyList a
new x xs =
    NonEmptyList x xs

toList : NonEmptyList a -> List a
toList (NonEmptyList x xs) =
    x :: xs

map : (a -> b) -> NonEmptyList a -> NonEmptyList b
map f (NonEmptyList x xs) =
    NonEmptyList (f x) (List.map f xs)

head : NonEmptyList a -> a
head (NonEmptyList x _) =
    x

tail : NonEmptyList a -> List a
tail (NonEmptyList _ xs) =
    xs