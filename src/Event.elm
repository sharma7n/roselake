module Event exposing
    ( Event
    , byId
    , generator
    )

import Random

import NonEmptyList exposing (NonEmptyList)

type alias Event =
    { id : String
    , name : String
    , description : String
    , choices : NonEmptyList Choice
    }

type alias Choice =
    { description : String
    }

byId : String -> Event
byId id =
    case id of
        _ ->
            { id = "null"
            , name = "Null Event"
            , description = "Null Event"
            , choices = NonEmptyList.new { description = "Null Choice" } []
            }

generator : Random.Generator Event
generator =
    Random.weighted
        ( 1, byId "null" )
        []