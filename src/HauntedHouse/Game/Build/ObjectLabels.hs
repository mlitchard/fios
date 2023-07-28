module HauntedHouse.Game.Build.ObjectLabels where 

import HauntedHouse.Game.Model.Mapping
import HauntedHouse.Game.Model.World  (Object)
import HauntedHouse.Tokenizer         (Lexeme(SINK,SHELF, CABINET))

sink :: Label Object
sink = Label SINK 

shelf :: Label Object 
shelf = Label SHELF 

cabinet :: Label Object 
cabinet = Label CABINET