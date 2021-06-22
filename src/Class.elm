module Class exposing
  ( new
  , default
  , byId
  , all
  , Class
  , generator
  )

import Random

import Util

type alias Class =
  { id : String
  , name : String
  }

new : String -> Class
new name =
  { id = String.toLower name
  , name = name
  }

default : Class
default =
  new "Null"

byId : String -> Class
byId =
  Util.getById all default

all : List Class
all =
  [ new "Knight"
  , new "Brawler"
  , new "Fencer"
  , new "Archer"
  , new "Ranger"
  , new "Aerialist"
  , new "Mariner"
  , new "Cleric"
  , new "Mage"
  , new "Artificer"
  , new "Thief"
  , new "Bard"
  , new "Dancer"
  , new "Jester"
  ]

generator : Random.Generator Class
generator =
  Random.weighted
    ( 0, byId "" )
    [ (1, byId "knight" )
    , (1, byId "brawler" )
    , (1, byId "fencer" )
    , (1, byId "archer" )
    , (1, byId "aerialist" )
    , (1, byId "mariner" )
    , (1, byId "cleric" )
    , (1, byId "mage" )
    , (1, byId "artificer" )
    , (1, byId "thief" )
    , (1, byId "bard" )
    , (1, byId "dancer" )
    , (1, byId "jester" )
    ]