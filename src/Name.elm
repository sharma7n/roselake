module Name exposing
    ( generator
    )

import Random

import Util

generator : Random.Generator String
generator =
    Util.uniformGenerator "" all

all : List String
all =
    [ "Akagi"
    , "Kaiji"
    , "Ippo"
    , "Takamura"
    ]