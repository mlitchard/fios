module HauntedHouse.Game.World.InitState where

import HauntedHouse.Game.World (World)

type WorldT = ExceptT Text (StateT World IO)



                


