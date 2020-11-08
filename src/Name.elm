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
    -- HUNTER X HUNTER
    , "Gon"
    , "Killua"
    , "Kurapica"
    , "Leorio"
    , "Hisoka"
    , "Ging"
    , "Meruem"
    , "Komugi"
    -- JOJO'S BIZARRE ADVENTURE
    , "Jonathan"
    , "Dio"
    , "Joseph"
    , "Caesar"
    , "Lisa Lisa"
    , "Santana"
    , "Wamuu"
    , "Esidisi"
    , "Kars"
    , "Jotaro"
    , "Avdol"
    , "Kakyoin"
    , "Polnareff"
    , "Iggy"
    , "Josuke"
    , "Koichi"
    , "Okuyasu"
    , "Kira"
    , "Giorno"
    , "Bucciarati"
    , "Narancia"
    , "Abbachio"
    , "Mista"
    , "Trish"
    , "Panacotta"
    , "Diavolo"
    -- CHRONO TRIGGER
    , "Chrono"
    , "Lucca"
    , "Marle"
    , "Frog"
    , "Robo"
    , "Ayla"
    , "Magus"
    -- SEIKEN DENSETSU 3
    , "Duran"
    , "Angela"
    , "Hawkeye"
    , "Riesz"
    , "Kevin"
    , "Charlotte"
    -- EARTHBOUND
    , "Ness"
    , "Paula"
    , "Jeff"
    , "Poo"
    ]