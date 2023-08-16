module HauntedHouse.Game.Model.Display where

import Data.Map.Strict qualified (null, elems)
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Model.World
        (ContainedIn (..), ContainedOn (..), ContainedBy (..), Interface (..)
        , Object (..), Containment (..))
import HauntedHouse.Game.Model.Mapping (ContainerMap(..))
import HauntedHouse.Game.Object (displayObjectM, getObjectM)
import HauntedHouse.Internal ( throwMaybeM, throwRightM, throwLeftM)
import qualified Data.List.NonEmpty
import Data.These

class Display a where
    displayScene :: a -> GameStateExceptT ()

    display :: a -> GameStateExceptT ()

{-

data ContainedIn = ContainedIn 
  { _interface' :: Interface Containment 
  , _containedIn' :: ContainerMap Object
  } deriving stock (Eq,Ord,Show)

-}

instance Display ContainedIn where

  displayScene :: ContainedIn -> GameStateExceptT ()
  displayScene (ContainedIn Open (ContainerMap containedIn)) = do
    if Data.Map.Strict.null containedIn
      then print ("You don't see anything inside" :: Text)
      else print ("You see something inside" :: Text)
  displayScene (ContainedIn (Closed _) _) = print ("It's closed" :: Text)

  display :: ContainedIn -> GameStateExceptT ()
  display (ContainedIn Open (ContainerMap containedIn)) =
    mapM_ displayObjectM flatten
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
    mapM_ displayObjectM flatten
    where
      flatten = concatMap Data.List.NonEmpty.toList
                  $ Data.Map.Strict.elems containedOn

{-

data Object = Object
  { _shortName'     :: Text
  , _moveability'   :: Moveability
  , _containment'   :: Maybe (Either Containment Portal)
  , _odescription'  :: Text
  , _descriptors'   :: [Label Adjective]
  } deriving stock Show

-}

instance Display ContainedBy where
  displayScene :: ContainedBy -> GameStateExceptT ()
  displayScene (ContainedBy _) = pass

  display :: ContainedBy -> GameStateExceptT ()
  display (ContainedBy gid) = do
    object <- getObjectM gid
    these' <- _unContainment' 
                <$> (throwRightM "Not a container" 
                      =<< throwMaybeM "Not contained by" (_containment' object))
  
  {-
  data ContainedIn = ContainedIn 
  { _interface' :: Interface Containment 
  , _containedIn' :: ContainerMap Object
  } deriving stock (Eq,Ord,Show)
  -}
    case these' of 
      This containedIn -> pass 
      That e -> do 
                  containedOn <- throwRightM "Not contained On" e
                  pass
      These containedIn _ -> pass
      
    pass