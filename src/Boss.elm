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
    , showBoss : Bool
    }

new : String -> MonsterTemplate -> (Boss -> Boss) -> Boss
new name monsterTemplate f =
    { id = Util.kebabify name
    , name = name
    , monsterTemplate = monsterTemplate
    , showBoss = False
    }
        |> f

default : Boss
default =
    new "Null Boss" (MonsterTemplate.byId "null") (\x -> x)

byId : String -> Boss
byId =
    Util.getById all default

all : List Boss
all =
    [ new "Pumpkin Wraith" (MonsterTemplate.byId "pumpkin-wraith") (\b ->
        b
      )
    ]