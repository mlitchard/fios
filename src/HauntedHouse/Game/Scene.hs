module HauntedHouse.Game.Scene where

import Data.List.NonEmpty ( NonEmpty, singleton )

import HauntedHouse.Game.Model (Scene (..), GameStateExceptT)
import HauntedHouse.Game.Model.World (Location (..), Objects (..))
import HauntedHouse.Game.World (getObjectM, getExitM)
import HauntedHouse.Internal (throwMaybeM)

{-

data Location = Location
  { _title'       :: Text
  , _description' :: Text
  , _objects'     :: Maybe Objects
  , _directions'  :: Maybe ExitGIDMap
  } deriving stock Show

data Scene = Scene
  {_roomTitle'         :: Text
  , _roomDescription'  :: Text
  , _anchoredObjects'  :: Data.List.NonEmpty.NonEmpty Text
  , _visibleContained' :: Data.List.NonEmpty.NonEmpty Text
  , _visibleExits'     :: Data.List.NonEmpty.NonEmpty Text
  } deriving stock Show

-}
{-
fromLocationM :: Location -> GameStateExceptT ()
fromLocationM (Location title description mObjects mDirections ) = do
  _ <- case mDirections of 
        Nothing -> noExits 
        Just (Exit)   ->   
  _ <- case mObjects of 
        Nothing -> emptyRoom 
        Just (Objects objectGIDs) -> do 
          objects <- mapM_ getObjectM objectGIDs
          pass 
-}
{-
  let pscene = partialScene (fromMaybe emptyRoom mObjects)
      exits = fromMaybe noExits mDirections  
      scene = pscene{_visibleExits' = exits}
  where
    partialScene :: Objects -> Scene
    partialScene (Objects object) 
-}
  {-
  let visibleExits = processDirections directions
  in case mObjects of 
      Nothing -> emptyRoom
      Just objects -> Scene { _roomTitle' = title
                            , _roomDescription'   = description
                            , _anchoredObjects'   = anchoredObjects
                            , _visibleContained'  = visibleContained
                            , _visibleExits'      = visibleExits
                            }
        where 
          visibleContained :: Data.List.NonEmpty.NonEmpty Text
          visibleContained = processVisibleContained objects

          anchoredObjects :: Data.List.NonEmpty.NonEmpty Text
          anchoredObjects = processAnchoredObjects objects  
  where
    emptyRoom = Scene title description empty empty visibleExits

processAnchoredObjects :: Objects -> Data.List.NonEmpty.NonEmpty Text
processAnchoredObjects (Objects _objectMap) = _

processVisibleContained :: Objects -> Data.List.NonEmpty.NonEmpty Text
processVisibleContained (Objects _objectMap) = _ 

processDirections :: 
-}




