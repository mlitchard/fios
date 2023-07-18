module HauntedHouse.Game.Object (
  module HauntedHouse.Game.Object 
, module HauntedHouse.Game.Object.Container 
, module HauntedHouse.Game.Object.Domain 

)where

import HauntedHouse.Game.Object.Container
import HauntedHouse.Game.Object.Domain
import Data.Text qualified (empty) 
import HauntedHouse.Game.Object.Container.Domain (Moveable(..))

defaultObject :: Object 
defaultObject = Object 
  {
    _container = Nothing
  , _containedBy = Nothing
  , _moveability = NotMovable
  , _odescription = Data.Text.empty 
  }
