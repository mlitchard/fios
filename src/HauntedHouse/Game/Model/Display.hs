{-# OPTIONS_GHC -Wno-orphans #-}
module HauntedHouse.Game.Model.Display where

import Data.Map.Strict qualified (null, elems, toList)

import HauntedHouse.Game.Model.World
        
import HauntedHouse.Game.Model.Mapping (ContainerMap(..), NeighborMap (..))
import HauntedHouse.Game.Object (getObjectM)

import qualified Data.List.NonEmpty
import Data.These
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Condition (Proximity, Perceptibility (..))
{-
data DisplayRelation = DisplayRelation
  { _proximity'         :: Proximity
  , _proximitObjects'   :: Data.List.NonEmpty.NonEmpty (GID Object)
  , _proximitShortName' :: Text
  }

class Display a where
    displayScene :: a -> GameStateExceptT ()

    display :: a -> GameStateExceptT ()

{-

data Object = Object {
    _shortName'       :: Text
  , _odescription'    :: Text
  , _descriptives'    :: [Label Adjective]
  , _metaConditions'  :: [MetaCondition]
}

data MetaCondition = MetaCondition {
   _condition :: Condition
  , _setCondition :: GameStateExceptT ()
  }

  data Condition
  = Mobility' Moveability
  | Perceptibility' Perceptibility
  | Either Proximity (AnchoredTo Object)
  | Nexus' Nexus 
  | Inventory
      deriving stock (Show,Eq,Ord)
-}

 

instance Display (GID Object) where

  displayScene :: GID Object -> GameStateExceptT ()
  displayScene gid = pass
    
     
    

  display = displayScene
{-
instance Display (GID Object, Neighbors) where

  displayScene :: (GID Object, Neighbors) -> GameStateExceptT ()
  displayScene (objectGID, Neighbors (NeighborMap relations)) = do
    (Object shortName _ mContainment description _) <- getObjectM objectGID
    print shortName
    print description
    whenJust mContainment (either display display)
    mapM_ display
      $ toDisplayRelation shortName
      <$> Data.Map.Strict.toList relations
    where
      toDisplayRelation shortName (proximity, proximitObjects) = DisplayRelation
        {_proximity' = proximity
        , _proximitObjects' = proximitObjects
        , _proximitShortName' = shortName}

  display = displayScene
-}
instance Display Portal where
  displayScene :: Portal -> GameStateExceptT ()
  displayScene (Portal _ interface) =
    case interface of
      Open       -> print ("It's open" :: Text)
      (Closed _) -> print ("It's closed" :: Text)

  display = displayScene

{-
newtype Containment = Containment 
  {_unContainment' :: These ContainedIn (Either ContainedOn ContainedBy)} 
    deriving stock (Eq, Ord, Show)
-}
instance Display Containment where

  displayScene :: Containment -> GameStateExceptT ()
  displayScene containment = case _unContainment' containment of
    (This containedIn) -> display containedIn
    (That onOrBy)      -> either display display onOrBy
    (These containedIn onOrBy) -> do
                                    display containedIn
                                    either display display onOrBy

  display = displayScene

instance Display DisplayRelation where

  displayScene :: DisplayRelation -> GameStateExceptT ()
  displayScene (DisplayRelation proximity gidObjects shortName) = do
    print (fromProximity proximity <> " " <> shortName)
    mapM_ display gidObjects

  display = displayScene

instance Display ContainedIn where

  displayScene :: ContainedIn -> GameStateExceptT ()
  displayScene (ContainedIn Open (ContainerMap containedIn)) = do
    if Data.Map.Strict.null containedIn
      then print ("You don't see anything inside" :: Text)
      else print ("You see something inside" :: Text)
  displayScene (ContainedIn (Closed _) _) = print ("It's closed" :: Text)

  display :: ContainedIn -> GameStateExceptT ()
  display (ContainedIn Open (ContainerMap containedIn)) =
    mapM_ display flatten
    where
      flatten = concatMap Data.List.NonEmpty.toList
                  $ Data.Map.Strict.elems containedIn
  display (ContainedIn (Closed _) _ ) = print ("It's closed" :: Text)

instance Display ContainedOn where

  displayScene :: ContainedOn -> GameStateExceptT ()
  displayScene (ContainedOn (ContainerMap containedOn)) = do
    if null containedOn
      then print ("There's nothing on it" :: Text)
      else print ("There is something on it" :: Text)

  display :: ContainedOn -> GameStateExceptT ()
  display (ContainedOn (ContainerMap containedOn)) =
    mapM_ display flatten
    where
      flatten = concatMap Data.List.NonEmpty.toList
                  $ Data.Map.Strict.elems containedOn


instance Display ContainedBy where
-- these :: (a -> c) -> (b -> c) -> (a -> b -> c) -> These a b -> c
  displayScene :: ContainedBy -> GameStateExceptT ()
  displayScene (ContainedBy containedBy objectContained) = do
    options <- _unContainment' <$> (throwRightM "Not a container"
                  =<< throwMaybeM "supposed to be container but isn't" . _containment'
                  =<< getObjectM containedBy)
    case options of
      (This containedIn) -> doContainedIn containedIn
      (That onOrBy)      -> either display display onOrBy
      (These containedIn onOrBy) -> doContainedIn containedIn 
                                      >> either display display onOrBy
      where 
        doContainedIn containedIn = do
          isVisible' <- isVisible containedIn
          if isVisible'
            then displayScene objectContained 
            else print ("You can't see that" :: Text)

  display = displayScene

instance VisibleObject ContainedIn where
  isVisible (ContainedIn Open _) = pure True
  isVisible _                    = pure False

instance VisibleObject ContainedOn where
  isVisible _ = pure True

instance VisibleObject ContainedBy where
  isVisible (ContainedBy gid _) = do
    either isVisible isVisible
      =<< throwMaybeM "Not a container" . _containment'
      =<< getObjectM gid

instance VisibleObject Portal where
  isVisible _ = pure True 

instance VisibleObject Containment where
  isVisible (Containment containment) =
    these isVisible 
            (either isVisible isVisible ) 
            (\x y -> isVisible x >> either isVisible isVisible y) 
            containment

instance Display RoomAnchors where

  displayScene :: RoomAnchors -> GameStateExceptT ()
  displayScene (RoomAnchors roomAnchorMap) = do
    mapM_ displayScene $ Data.Map.Strict.toList roomAnchorMap
  
  display :: RoomAnchors -> GameStateExceptT ()
  display = displayScene

instance Display (RoomAnchor, ObjectAnchors) where

  displayScene :: (RoomAnchor, ObjectAnchors) -> GameStateExceptT ()
  displayScene (key, ObjectAnchors objectRelationsMap) = do
    liftIO 
      $ print (("In the " :: Text) <> directionFromRoomAnchor key <> " you see")
    mapM_ displayScene $ Data.Map.Strict.toList objectRelationsMap
  
  display = displayScene
  -}