module HauntedHouse.Game.World.Locations.Kitchen where

import HauntedHouse.Game.Location.LocationData (LocationData (..))
import HauntedHouse.Game.Object
import HauntedHouse.Tokenizer (Lexeme(..))

{-
data LocationData = LocationData
  { _description :: Text
  , _objectNameMaps :: ObjectNameMap 
  }

-}
kitchen :: LocationData
kitchen = LocationData {
  _description = "It's a small kitchen"
, _objectNameMap = Nothing 
, _exits = Nothing  
}

kitchenObjectNames :: NonEmpty ObjectName
kitchenObjectNames = fromList [
  kitchenSinkCabinetName
  ,kitchenSinkName
  ,kitchenShelfCabinetBelowName
  ,kitchenShelfCabinetAboveName
  ,kitchenMarqueeName]

{-
data Object = Object
  { _container :: ContainerState 
  , _contained :: Maybe ObjectName
  , _moveability :: Moveable
  , _odescription :: Text
  }
  newtype ContainerState 
  = ContainerState (AttachedTo, Maybe (These Container Shelf)) 
data AttachedTo
  = AttachedToAgent (GID AgentName)
  | AttachedToObject (GID ObjectName)
  | AttachedToLocation (GID LocationName)
  
-}
kitchenSinkCabinetName :: ObjectName
kitchenSinkCabinetName = ObjectName CABINET

kitchenSinkName :: ObjectName
kitchenSinkName = ObjectName SINK

kitchenShelfCabinetBelowName :: ObjectName
kitchenShelfCabinetBelowName = ObjectName CABINET

kitchenShelfCabinetAboveName :: ObjectName
kitchenShelfCabinetAboveName = ObjectName CABINET

kitchenMarqueeName :: ObjectName
kitchenMarqueeName = ObjectName MARQUEE
