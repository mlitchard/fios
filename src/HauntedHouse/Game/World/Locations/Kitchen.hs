module HauntedHouse.Game.World.Locations.Kitchen where

import HauntedHouse.Game.World.Locations.Kitchen.KitchenSink
import HauntedHouse.Game.World.Locations.Kitchen.KitchenSinkCabinets 
import HauntedHouse.Game.Location.LocationData (LocationData (..))
import HauntedHouse.Game.Object
import HauntedHouse.Tokenizer (Lexeme(..))

{-
data LocationData = LocationData
  { _description :: Text
  , _objectNameMaps :: ObjectNameMap 
  }

-}
{-
kitchenObjectNames = fromList [
  (kitchenSinkCabinetAboveName,kitchenSinkCabinetAbove)
  ,(kitchenSinkCabinetBelowName,kitchenSinkCabinetBelow)
  ,(kitchenSinkName,kitchenSink)
  ,(kitchenShelfCabinetBelowName,kitchenShelfCabinetBelow)
  ,(kitchenShelfCabinetAboveName,kitchenShelfCabinetAbove)]
 -- ,(kitchenMarqueeName,kitchenMarquee)]
-}
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
{-




kitchenSinkName :: ObjectName
kitchenSinkName = ObjectName SINK



kitchenShelfCabinetAboveName :: ObjectName
kitchenShelfCabinetAboveName = ObjectName CABINET

kitchenMarqueeName :: ObjectName
kitchenMarqueeName = ObjectName MARQUEE
-}