{-# OPTIONS_GHC -Wno-orphans #-}
module HauntedHouse.Game.Model.Display where

import Data.Map.Strict qualified (null, elems, toList)
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Model.World
        (ContainedIn (..), ContainedOn (..), ContainedBy (..), Interface (..)
        , Object (..), Containment (..), Proximity, Neighbors (..), Portal (..), fromProximity)
import HauntedHouse.Game.Model.Mapping (ContainerMap(..), NeighborMap (..))
import HauntedHouse.Game.Object (getObjectM)
import HauntedHouse.Internal
        ( throwMaybeM, throwRightM, throwLeftM, VisibleObject (..))
import qualified Data.List.NonEmpty
import Data.These
import HauntedHouse.Game.Model.GID (GID)

data DisplayRelation = DisplayRelation
  { _proximity'         :: Proximity
  , _proximitObjects'   :: Data.List.NonEmpty.NonEmpty (GID Object)
  , _proximitShortName' :: Text
  }

class Display a where
    displayScene :: a -> GameStateExceptT ()

    display :: a -> GameStateExceptT ()

instance Display (GID Object) where

  displayScene :: GID Object -> GameStateExceptT ()
  displayScene gid = do
    (Object shortname _ mContainment _ _ ) <- getObjectM gid
    print shortname
    whenJust mContainment (`whenLeft_` display)

  display = displayScene

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

{-
newtype Containment = Containment 
  { _unContainment' :: These ContainedIn (Either ContainedOn ContainedBy)}
    deriving stock (Eq, Ord, Show)
-}
instance Display ContainedBy where
-- these :: (a -> c) -> (b -> c) -> (a -> b -> c) -> These a b -> c
  displayScene :: ContainedBy -> GameStateExceptT ()
  displayScene (ContainedBy containedBy objectContained) = do
    options <- _unContainment' <$> (throwRightM "Not a container"
                  =<< throwMaybeM "supposed to be container but isn't" . _containment'
                  =<< getObjectM containedBy)
    case options of
      (This containedIn) -> do
                              isVisible' <- isVisible containedIn
                              if isVisible'
                                then displayScene objectContained 
                                else print ("You can't see that" :: Text)
      (That onOrBy)      -> pass
      _                  -> pass

  display = displayScene

instance VisibleObject ContainedIn where
  isVisible (ContainedIn Open _) = pure True
  isVisible _                    = pure False

instance VisibleObject ContainedOn where
  isVisible _ = pure True
{-

data ContainedBy = ContainedBy 
  { _containedBy' :: GID Object
  , _objectContained' :: GID Object
  } deriving stock (Eq,Ord,Show)

-}
instance VisibleObject ContainedBy where
  isVisible (ContainedBy gid _) = do
    either isVisible isVisible
      =<< throwMaybeM "Not a container" . _containment'
      =<< getObjectM gid

{-
{ _unContainment' :: These ContainedIn (Either ContainedOn ContainedBy)}
-}
instance VisibleObject Containment where
  isVisible (Containment containment) =
    these isVisible 
            (either isVisible isVisible ) 
            (\x y -> isVisible x >> either isVisible isVisible y) 
            containment
