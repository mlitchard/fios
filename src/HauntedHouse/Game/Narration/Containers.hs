module HauntedHouse.Game.Narration.Containers where

import Data.Map.Strict (null)
import HauntedHouse.Game.Model (GameStateExceptT)
import HauntedHouse.Game.Model.World
        (Portal (..), ContainedIn (..), ContainedOn (..), Interface (..), Containment)
import Data.These (These(..))
import HauntedHouse.Game.Model.Mapping (ContainerMap(..))

displayPortal :: Portal -> GameStateExceptT ()
displayPortal (Portal _ interface) =
  case interface of
    Open       -> print ("It's open" :: String)
    (Closed _) -> print ("It's closed" :: String)

{-

data Threes a b c 
  = ThreeWay a b c 
  | TwoWayAB a b 
  | TwoWayAC a c 
  | TwoWayBC b c 
  | OneWayA a
  | OneWayB b 
  | OneWayC c 
    deriving stock (Show, Eq, Ord)

-}
displayContainment :: Containment -> GameStateExceptT () 
displayContainment _ = pass 
{-
displayContainment :: Container  
                        -> GameStateExceptT ()
displayContainment (Container contained) = do
  case contained of
    OneWayA containedIn -> displayContainedInInitial containedIn
    OneWayB containedOn -> displayContainedOnInitial containedOn
    OneWayC containedBy -> displayContainedOnInitial containedBy
    ThreeWay containedIn containedOn containedBy -> do
      isClosed <- containerClosed containedBy
      if isClosed then pass else display
-}
displayContainedIn :: ContainedIn -> GameStateExceptT () 
displayContainedIn _ = pass 

displayContainedInInitial :: ContainedIn -> GameStateExceptT ()
displayContainedInInitial (ContainedIn Open (ContainerMap containerMap)) = do
  if Data.Map.Strict.null containerMap
    then print ("You don't see anything inside." :: Text)
    else print ("There's something inside." :: Text)
displayContainedInInitial (ContainedIn (Closed _) _) = 
  print ("It's closed." :: Text)

displayContainedOn :: ContainedOn -> GameStateExceptT () 
displayContainedOn _ = pass 

displayContainedOnInitial :: ContainedOn -> GameStateExceptT ()
displayContainedOnInitial (ContainedOn (ContainerMap containerMap)) = do 
  if Data.Map.Strict.null containerMap
    then print ("There's nothing on it." :: Text)
    else print ("You see some things on it." :: Text)
{-
displayContainedBy :: ContainedBy -> GameStateExceptT () 
displayContainedBy _ = pass 
-}