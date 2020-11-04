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
    -- KAIJI
    [ "Kaiji"
    -- AKAGI
    , "Akagi"
    -- HAJIME NO IPPO
    , "Ippo"
    , "Takamura"
    , "Aoki"
    , "Kimura"
    -- ONE PIECE
    , "Luffy"
    , "Zoro"
    , "Nami"
    , "Usopp"
    , "Sanji"
    , "Chopper"
    , "Robin"
    , "Franky"
    , "Brook"
    , "Jinbei"
    -- FINAL FANTASY IV
    , "Cecil"
    , "Kain"
    , "Rydia"
    , "Tellah"
    , "Edward"
    , "Rosa"
    , "Yang"
    , "Palom"
    , "Porom"
    , "Cid"
    , "Edge"
    , "FuSoYa"
    -- FINAL FANTASY VI
    , "Terra"
    , "Locke"
    , "Edgar"
    , "Sabin"
    , "Banon"
    , "Celes"
    , "Shadow"
    , "Cyan"
    , "Gau"
    , "Setzer"
    , "Mog"
    , "Umaro"
    , "Gogo"
    -- FINAL FANTASY IX
    , "Zidane"
    , "Cinna"
    , "Blank"
    , "Marcus"
    , "Vivi"
    , "Garnet"
    , "Steiner"
    , "Freya"
    , "Quina"
    , "Eiko"
    , "Amarant"
    ]