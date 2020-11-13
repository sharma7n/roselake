module Boss exposing
    ( Boss
    , byId
    )

import Util
import MonsterTemplate exposing (MonsterTemplate)

type alias Boss =
    { id : String
    , name : String
    , monsterTemplate : MonsterTemplate
    }

default : Boss
default =
    { id = "null"
    , name = "Null Boss"
    , monsterTemplate = MonsterTemplate.byId "null"
    }

byId : String -> Boss
byId =
    Util.getById all default

all : List Boss
all =
    let
        ogopogo =
            { default
                | id = "ogopogo"
                , name = "Ogopogo"
                , monsterTemplate = MonsterTemplate.byId "ogopogo"
            }
    in
    [ ogopogo
    ]