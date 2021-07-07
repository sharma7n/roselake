module Element exposing
  ( Element(..)
  , toString
  )

type Element
  = Fire
  | Ice
  | Water
  | Lightning
  | Wind
  | Earth
  | Rock
  | Metal
  | Poison
  | Leaf
  | Fighting
  | Psychic
  | Life
  | Death
  | Image
  | Sound
  | Blood
  | Spirit
  | Magic
  | Force
  | Sun
  | Moon
  | Holy
  | Evil
  | Law
  | Chaos
  | Luck
  | Space
  | Time
  | Void
  | Nuclear
  | Prism
  | Ultima

type alias ElementArg a =
  { fire : a
  , ice : a
  , water : a
  , lightning : a
  , wind : a
  , earth : a
  , rock : a
  , metal : a
  , poison : a
  , leaf : a
  , fighting : a
  , psychic : a
  , life : a
  , death : a
  , image : a
  , sound : a
  , blood : a
  , spirit : a
  , magic : a
  , force : a
  , sun : a
  , moon : a
  , holy : a
  , evil : a
  , law : a
  , chaos : a
  , luck : a
  , space : a
  , time : a
  , void : a
  , nuclear : a
  , prism : a
  , ultima : a
  }

fold : Element -> ElementArg a -> a
fold e arg =
  case e of
    Fire ->
      arg.fire
    
    Ice ->
      arg.ice
    
    Water ->
      arg.water
    
    Lightning ->
      arg.lightning
    
    Wind ->
      arg.wind
    
    Earth ->
      arg.earth
    
    Rock ->
      arg.rock
    
    Metal ->
      arg.metal
    
    Poison ->
      arg.poison
    
    Leaf ->
      arg.leaf
    
    Fighting ->
      arg.fighting
    
    Psychic ->
      arg.psychic
    
    Life ->
      arg.life
    
    Death ->
      arg.death
    
    Image ->
      arg.image
    
    Sound ->
      arg.sound
    
    Blood ->
      arg.blood
    
    Spirit ->
      arg.spirit
    
    Magic ->
      arg.magic
    
    Force ->
      arg.force
    
    Sun ->
      arg.sun
    
    Moon ->
      arg.moon
    
    Holy ->
      arg.holy
    
    Evil ->
      arg.evil
    
    Law ->
      arg.law
    
    Chaos ->
      arg.chaos
    
    Luck ->
      arg.luck
    
    Space ->
      arg.space
    
    Time ->
      arg.time
    
    Void ->
      arg.void
    
    Nuclear ->
      arg.nuclear
    
    Prism ->
      arg.prism
    
    Ultima ->
      arg.ultima

toString : Element -> String
toString e =
  { fire = "Fire"
  , ice = "Ice"
  , water = "Water"
  , lightning = "Lightning"
  , wind = "Wind"
  , earth = "Earth"
  , rock = "Rock"
  , metal = "Metal"
  , poison = "Poison"
  , leaf = "Leaf"
  , fighting = "Fighting"
  , psychic = "Psychic"
  , life = "Life"
  , death = "Death"
  , image = "Image"
  , sound = "Sound"
  , blood = "Blood"
  , spirit = "Spirit"
  , magic = "Magic"
  , force = "Force"
  , sun = "Sun"
  , moon = "Moon"
  , holy = "Holy"
  , evil = "Evil"
  , law = "Law"
  , chaos = "Chaos"
  , luck = "Luck"
  , space = "Space"
  , time = "Time"
  , void = "Void"
  , nuclear = "Nuclear"
  , prism = "Prism"
  , ultima = "Ultima"
  }
    |> fold e