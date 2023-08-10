module HauntedHouse.Game.Narration.Containers where

import Data.Map.Strict (null)
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Model.World
        (Portal (..), Container (Container), ContainedIn (..), ContainedOn (..), Interface (..))
import Data.These (These(..))
import HauntedHouse.Game.Model.Mapping (ContainerMap(..))

displayPortal :: Portal -> GameStateExceptT ()
displayPortal (Portal _ interface) =
  case interface of
    Open       -> print ("It's open" :: String)
    (Closed _) -> print ("It's closed" :: String)

displayContainer :: Container -> GameStateExceptT ()
displayContainer (Container contained) = do
  case contained of
    This containedIn -> displayContainedInInitial containedIn
    That containedOn -> displayContainedOnInitial containedOn
    These containedIn containedOn -> do
                                        displayContainedInInitial containedIn
                                        displayContainedOnInitial containedOn

displayContainedInInitial :: ContainedIn -> GameStateExceptT ()
displayContainedInInitial (ContainedIn Open (ContainerMap containerMap)) = do
  if Data.Map.Strict.null containerMap
    then print ("You don't see anything inside.\n" :: String)
    else print ("There's something inside.\n" :: String)
displayContainedInInitial (ContainedIn (Closed _) _) = 
  print ("It's closed.\n" :: String)

displayContainedOnInitial :: ContainedOn -> GameStateExceptT ()
displayContainedOnInitial (ContainedOn (ContainerMap containerMap)) = do 
  if Data.Map.Strict.null containerMap
    then print ("There's nothing on it.\n" :: String)
    else print ("You see some things on it.\n" :: String)