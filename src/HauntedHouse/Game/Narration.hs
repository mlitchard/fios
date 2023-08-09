module HauntedHouse.Game.Narration where

import HauntedHouse.Game.Model
        (Narration (..), GameStateExceptT, GameState (..), Scene (..))
import HauntedHouse.Game.Model.World
        (Location (..), Objects (..), Object (..)
        , Moveability (..), RoomAnchors (..), RoomAnchor, ObjectAnchors (..)
        , Neighbors (..), Portal (..), Container (..), Proximity (..)
        , Interface (..), ContainedIn (..), ContainedOn (..))
import HauntedHouse.Game.World (getObjectM)
import qualified Data.List.NonEmpty (filter, partition, NonEmpty)
import Data.List.NonEmpty ((<|))
import qualified Data.List
import HauntedHouse.Tokenizer (objects)
import qualified Data.Map.Strict
import HauntedHouse.Game.Model.GID (GID)
import Data.Aeson (object)
import HauntedHouse.Game.Model.Mapping (NeighborMap(..))
import Data.These (These(..))

updateNarration :: Narration -> GameStateExceptT ()
updateNarration narration =  modify' (\g -> g {_narration' = narration})

{-

data Location = Location
  { _title'           :: Text
  , _description'     :: Text
  , _anchoredObjects' :: RoomAnchors
  , _floorInventory'  :: Maybe Objects
  , _directions'      :: Maybe ExitGIDMap
  } deriving stock Show

newtype ObjectAnchors
          = ObjectAnchors {
              _unObjectAnchors :: Data.Map.Strict.Map (GID Object) Relations
            } deriving stock Show
newtype RoomAnchors
          = RoomAnchors {
              _unRoomAnchors :: Data.Map.Strict.Map RoomAnchor ObjectAnchors
            } deriving stock Show
-}

displayScene :: Location -> GameStateExceptT ()
displayScene (Location title description anchored floor directions) = do
  liftIO $ print ("You are in the " <> title <> "\n")
  liftIO $ print (description <> "\n")
  liftIO $ print ("This is what you see\n" :: Text)
  --liftIO $ print $ maybe emptyRoom objectDescriptions objects
  where
    emptyRoom = "An Empty Room"

displayObjects :: RoomAnchors -> GameStateExceptT ()
displayObjects (RoomAnchors roomAnchorMap) = do
  mapM_ displayObjectByLocation $ Data.Map.Strict.toList roomAnchorMap
  pass

displayObjectByLocation :: (RoomAnchor, ObjectAnchors) -> GameStateExceptT ()
displayObjectByLocation (key, ObjectAnchors objectRelationsMap) = do
  liftIO $ print (("In the " :: String) <> show key <> " you see")
  mapM_ displayLocationArea $ Data.Map.Strict.toList objectRelationsMap

{-

newtype NeighborMap a b = NeighborMap {
  _unNeighborMap :: Data.Map.Strict.Map a (Data.List.NonEmpty.NonEmpty (GID b))
} deriving stock Show 

data Object = Object
  { _shortName'     :: Text
  , _moveability'   :: Moveability
  , _containment'   :: Maybe (Either Container Portal)
  , _odescription'  :: Text
  } deriving stock Show
-}
displayLocationArea :: (GID Object, Neighbors) -> GameStateExceptT ()
displayLocationArea (objectGID, Neighbors (NeighborMap relations)) = do
  object@(Object shortName _ mContainment description) <- getObjectM objectGID
  print (shortName <> "\n")
  print (description <> "\n")
  whenJust mContainment (either displayContainer displayPortal)
  mapM_ (displayRelations shortName) $ Data.Map.Strict.toList relations

displayPortal :: Portal -> GameStateExceptT ()
displayPortal _ = pass

{-

data Container = Container
  { _containerInterFace'  :: Interface Container
  , _contained'           :: These ContainedIn ContainedOn
  } deriving stock (Eq,Ord,Show)

newtype Container = Container {_unContainer :: These ContainedIn ContainedOn } 

newtype Interface a = Interface
  { _openState'  :: Maybe OpenClosed } deriving stock (Eq,Ord,Show)

newtype ContainedOn = ContainedOn {_unContainedOn' :: ContainerMap Object}
  deriving stock (Eq,Ord,Show)

newtype ContainedIn = ContainedIn {
  _interface :: Interface Container 
  _containedOn' :: ContainerMap Object}
  deriving stock (Eq,Ord,Show)
-}
displayContainer :: Container -> GameStateExceptT ()
displayContainer (Container contained) = do
  case contained of
    This containedIn -> displayContainedInInitial containedIn 
    That containedOn -> displayContainedOn containedOn 
    These containedIn containedOn -> do
                                        displayContainedInInitial containedIn
                                        displayContainedOn containedOn 

displayContainedInInitial :: ContainedIn -> GameStateExceptT ()
displayContainedInInitial containedIn = pass

displayContainedOn :: ContainedOn -> GameStateExceptT () 
displayContainedOn containedOn = pass 

displayRelations :: Text
                      -> Proximity
                      -> Data.List.NonEmpty.NonEmpty (GID Object)
                      -> GameStateExceptT ()
displayRelations shortName proximity gidObjects = do
  print (show proximity <> " " <> shortName)
  mapM_ displayObjectM gidObjects

{-
data Object = Object
  { _shortName'     :: Text
  , _moveability'   :: Moveability
  , _containment'   :: Maybe (Either Container Portal)
  , _odescription'  :: Text
  } deriving stock Show
-}

displayObjectM :: GID Object -> GameStateExceptT ()
displayObjectM gidObject = do
  (Object shortname _ mContainment _) <- getObjectM gidObject
  print shortname <> "\n"
  whenJust mContainment (`whenLeft_` displayContainer)
  pass