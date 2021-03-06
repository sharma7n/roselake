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
    , "Miyata"
    , "Mashiba"
    , "Sendou"
    , "Volg"
    , "Date"
    , "Ricardo"
    , "Sawamura"
    , "Alfredo"
    , "Antonio"
    -- MADOKA MAGICA
    , "Madoka"
    , "Mami"
    , "Homura"
    -- ONE PIECE / STRAW HAT PIRATES
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
    -- ONE PIECE / EAST BLUE
    , "Coby"
    , "Alvida"
    , "Helmeppo"
    , "Morgan"
    , "Buggy"
    , "Gaimon"
    , "Kaya"
    , "Kuro"
    , "Django"
    , "Zeff"
    , "Gin"
    , "Krieg"
    , "Nojiko"
    , "Genzo"
    , "Arlong"
    , "Smoker"
    , "Tashigi"
    , "Dragon"
    -- ONE PIECE / GRAND LINE
    , "Crocus"
    , "Laboon"
    , "Dory"
    , "Broggy"
    , "Galdino"
    , "Kureha"
    , "Dalton"
    , "Wapol"
    -- ONE PIECE / ALABASTA
    , "Cobra"
    , "Vivi"
    , "Crocodile"
    -- ONE PIECE / SKYPIEA
    , "Bellamy"
    , "Sarkiss"
    , "Mashira"
    , "Conis"
    , "Viper"
    , "Gan Fall"
    , "Enel"
    -- ONE PIECE / WATER 7
    , "Iceberg"
    , "Califa"
    , "Kokoro"
    , "Lucci"
    , "Spandam"
    -- ONE PIECE / THRILLER BARK
    , "Lola"
    , "Ryuma"
    , "Absalom"
    , "Perona"
    , "Moria"
    , "Kuma"
    -- ONE PIECE / SABADOY ARCHIPELAGO
    , "Kidd"
    , "Law"
    , "Bonney"
    , "Sentoumaru"
    , "Rayleigh"
    -- HUNTER X HUNTER
    , "Gon"
    , "Killua"
    , "Kurapica"
    , "Leorio"
    , "Hisoka"
    , "Ging"
    , "Meruem"
    , "Komugi"
    -- JOJO'S BIZARRE ADVENTURE I
    , "Jonathan"
    , "Dio"
    -- JOJO'S BIZARRE ADVENTURE II
    , "Joseph"
    , "Caesar"
    , "Lisa Lisa"
    , "Santana"
    , "Wamuu"
    , "Esidisi"
    , "Kars"
    -- JOJO'S BIZARRE ADVENTURE III
    , "Jotaro"
    , "Avdol"
    , "Kakyoin"
    , "Polnareff"
    , "Iggy"
    -- JOJO'S BIZARRE ADVENTURE IV
    , "Josuke"
    , "Koichi"
    , "Okuyasu"
    , "Kira"
    -- JOJO'S BIZARRE ADVENTURE V
    , "Giorno"
    , "Bucciarati"
    , "Narancia"
    , "Abbachio"
    , "Mista"
    , "Trish"
    , "Panacotta"
    , "Diavolo"
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
    , "Golbez"
    , "Zemus"
    -- FINAL FANTASY V
    , "Bartz"
    , "Lenna"
    , "Galuf"
    , "Faris"
    , "Krile"
    , "Exdeath"
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
    , "Kefka"
    , "Gestahl"
    -- FINAL FANTASY VII
    , "Cloud"
    , "Barret"
    , "Tifa"
    , "Aeris"
    , "Cait Sith"
    , "Cid"
    , "Yuffie"
    , "Vincent"
    , "Sephiroth"
    -- FINAL FANTASY VIII
    , "Squall"
    , "Quistis"
    , "Selphie"
    , "Zell"
    , "Rinoa"
    , "Irvine"
    , "Laguna"
    , "Kiros"
    , "Ward"
    , "Edea"
    , "Ultimecia"
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
    , "Garland"
    , "Kuja"
    -- FINAL FANTASY TACTICS
    , "Ramza"
    , "Agrias"
    , "Mustadio"
    , "Rapha"
    , "Marach"
    , "Meliadoul"
    , "Orlandu"
    , "Delita"
    , "Ajora"
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