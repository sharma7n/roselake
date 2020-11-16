module Boss exposing
    ( Boss
    , byId
    )

import Util
import MonsterTemplate exposing (MonsterTemplate)
import BossBehavior exposing (BossBehavior)

type alias Boss =
    { id : String
    , name : String
    , monsterTemplate : MonsterTemplate
    , showBoss : Bool
    , bossBehavior : BossBehavior
    }

new : String -> MonsterTemplate -> BossBehavior -> (Boss -> Boss) -> Boss
new name monsterTemplate bossBehavior f =
    { id = Util.kebabify name
    , name = name
    , monsterTemplate = monsterTemplate
    , showBoss = False
    , bossBehavior = bossBehavior
    }
        |> f

default : Boss
default =
    new "Null Boss" (MonsterTemplate.byId "null") BossBehavior.None (\x -> x)

byId : String -> Boss
byId =
    Util.getById all default

all : List Boss
all =
    [ new "Leviathan" (MonsterTemplate.byId "leviathan") BossBehavior.Leviathan (\b ->
        b
      )
    ]