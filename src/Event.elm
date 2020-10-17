module Event exposing
    ( Event
    , byId
    , generator
    )

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