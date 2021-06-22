module Scene exposing
    ( Scene(..)
    )

type Scene
    = Player
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
    | Inventory