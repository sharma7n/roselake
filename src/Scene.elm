module Scene exposing
    ( Scene(..)
    )

type Scene
    = Player
    | Essentia
    | LearnSelect
    | Equip
    | Home
    | ShopSelect
    | Shop
    | DungeonSelect
    | ExploreDungeon
    | BattleSelect
    | BattleMonsterLoadingIntent
    | BattleMonster
    | VictoryLoading
    | Victory
    | GameOver
    | Escaped
    | Town