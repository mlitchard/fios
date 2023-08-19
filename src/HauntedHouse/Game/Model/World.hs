module HauntedHouse.Game.Model.World where

import Data.List.NonEmpty qualified
import Data.Map.Strict (Map)
import HauntedHouse.Game.Model.GID
import HauntedHouse.Game.Model.Mapping

import HauntedHouse.Recognizer (Adjective)
import qualified Data.Text
import Data.These

data Object = Object
  { _shortName'     :: Text
  , _odescription'  :: Text
  , _conditions'    :: [Condition]
  , _descriptives'  :: [Label Adjective]
  } deriving stock Show

newtype Nexus = Nexus {_unNexus' :: Either Containment Portal}  
                        deriving stock (Eq, Ord,Show) 

data Condition
  = Mobility' Moveability
  | Perceptibility' Perceptibility
  | Nexus' Nexus
  | AnchoredTo' AnchoredTo 
  | Inventory 
     deriving stock (Eq, Ord, Show)


data Moveability 
  = Moveable 
  | NotMoveable AnchoredTo 
      deriving stock (Eq, Ord, Show)

data Perceptibility 
  = Perceptible 
  | Imperceptible 
      deriving stock (Eq, Ord, Show)

data Location = Location
  { _title'           :: Text
  , _description'     :: Text
  , _anchoredObjects' :: RoomAnchors
  , _anchoredTo'      :: AnchoredTo 
  , _floorInventory'  :: Maybe Objects
  , _objectLabelMap'  :: LabelToGIDListMapping Object Object
  , _directions'      :: Maybe ExitGIDMap
  } deriving stock Show

data RoomAnchor
  = NorthAnchor
  | SouthAnchor
  | WestAnchor
  | EastAnchor
  | NorthWestAnchor
  | NorthEastAnchor
  | SouthWestAnchor
  | SouthEastAnchor
  | CenterAnchor
    deriving stock (Show,Eq,Ord)

newtype ObjectAnchors
          = ObjectAnchors {
              _unObjectAnchors :: Data.Map.Strict.Map (GID Object) Neighbors
            } deriving stock Show

newtype AnchoredTo = AnchoredTo 
  { _unAnchoredTo' :: Data.Map.Strict.Map (GID Object) (GID Object,Proximity)} 
    deriving stock (Show, Eq, Ord) 

newtype RoomAnchors
          = RoomAnchors {
              _unRoomAnchors :: Data.Map.Strict.Map RoomAnchor ObjectAnchors
            } deriving stock Show

newtype ExitGIDMap
  = ExitGIDMap {_unExitGIDMap' :: LabelToGIDMapping Exit Object}
      deriving stock Show

data Interface a = Open | Closed LockState deriving stock (Eq,Ord,Show)

newtype Exit = Exit { _toLocation' :: GID Location} deriving stock Show

newtype Objects
  = Objects {_unObjects' :: Data.List.NonEmpty.NonEmpty (GID Object)}
      deriving stock Show

data World = World
  { _objectMap'         :: GIDToDataMapping Object
  , _locationMap'       :: GIDToDataMapping Location
  , _descriptiveMap'    :: LabelToGIDListMapping Adjective Object
  , _exitMap'           :: GIDToDataMapping Exit
  } deriving stock Show

data LockState 
  = Locked 
  | Unlocked 
  | Unlockable 
      deriving stock (Show, Eq, Ord)

data Proximity
  = PlacedOn
  | PlacedUnder
  | PlacedAbove
  | PlacedLeft
  | PlacedRight
  | PlacedFront 
  | PlacedBack
      deriving stock (Eq,Ord,Show)

fromProximity :: Proximity -> Text 
fromProximity proximity = 
  Data.Text.toLower . snd $ Data.Text.breakOnEnd "Placed" $ show proximity
  
newtype Neighbors = Neighbors 
  {_unNeighbors' :: NeighborMap Proximity Object} deriving stock Show 

newtype Containment = Containment 
  { _unContainment' :: These ContainedIn (Either ContainedOn ContainedBy)}
    deriving stock (Eq, Ord, Show)

data ContainedIn = ContainedIn 
  { _interface' :: Interface Containment 
  , _containedIn' :: ContainerMap Object
  } deriving stock (Eq,Ord,Show)

data ContainedBy = ContainedBy 
  { _containedBy' :: GID Object
  , _objectContained' :: GID Object
  } deriving stock (Eq,Ord,Show)

newtype ContainedOn = ContainedOn {_unContainedOn' :: ContainerMap Object}
  deriving stock (Eq,Ord,Show)

data Portal = Portal
  { _portalExit'      :: GID Exit
  , _portalInterface' :: Interface Portal
  } deriving stock (Eq,Ord,Show)


   