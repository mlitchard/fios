{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use one" #-}
module Build.Locations.Kitchen.Floor where

import Game.Model.World
import Build.ObjectTemplate (plantPotGID, kitchenFloorGID)
import Game.Model.Mapping
import qualified Data.Map.Strict
import qualified Data.List.NonEmpty
import Game.Model.Condition (Moveability(..), Perceptibility (..))
import Build.ObjectLabels (plantPotLabel)
import Tokenizer (Lexeme(..))
import Game.Actions.Look.StandardLook
import Game.Model.Display (updateEnvironmentM)
import Game.Model.GID
import Game.World (initContainerMapM)

-- Anchoring RoomAnchor

buildKitchenFloor :: GameStateExceptT ()
buildKitchenFloor = do
  world <- _world' <$> get
 -- world <- _world' <$> get
  let objectMap' :: GIDToDataMapping Object Object
      objectMap' =
        GIDToDataMapping $ Data.Map.Strict.insert kitchenFloorGID buildFloor
          $ (_unGIDToDataMapping' . _objectMap') world
  initContainerMapM kitchenFloorGID floorContainer
  modify' (\gs -> gs{_world' = world{_objectMap' = objectMap'}})

buildFloor :: Object
buildFloor = Object { 
    _shortName'         = "kitchen floor."
  , _entityLabel' = Label FLOOR
  , _odescription'    = [desc]
  , _descriptives'     = []
  , _moveability'     = NotMoveable
  , _perceptability'  = Perceptible
  , _orientation'     = orientation
  , _standardActions' = standardActions
}
  where
    desc = "A non-descipt tiled kitchen floor."
    orientation = Anchored CenterAnchor

floorContainer :: Container
floorContainer = 
  Container
    $ ContainerMap $ Data.Map.Strict.singleton plantPotLabel floorInv

floorInv :: GIDList Object
floorInv = Data.List.NonEmpty.singleton plantPotGID

standardActions :: StandardActions
standardActions = StandardActions { 
    _getAction' = getAction
  , _putAction' = putAction 
  , _lookAction' = lookAction
  , _openAction' = openAction
  , _closeAction' = closeAction 
  , _lockAction' = lockAction
  , _unlockAction' = unlockAction
  , _goAction' = goAction
}

closeAction :: CloseAction 
closeAction = CloseAction 
  {   _updateClose' = pass
    , _close' = const close
  }

close :: GameStateExceptT () 
close = updateEnvironmentM msg 
  where
    msg = "How are you going to close a floor ? Arble with a smidge of garble?"
goAction :: GoAction 
goAction = GoAction 
  {   _updateGo' = pass
    , _go' = const (const go)
  }
go :: GameStateExceptT ()
go = updateEnvironmentM msg 
  where
    msg = "While it might be possible to do that with some floors, "
            <> "you can't do that with this one."
unlockAction :: UnlockAction
unlockAction = UnlockAction 
  { _updateUnlock' = pass 
  , _unlock' = unlock
  }

unlock :: GameStateExceptT ()
unlock = updateEnvironmentM msg 
  where 
    msg = "Think hard about what you just tried to do, unlock the floor."
lockAction :: LockAction 
lockAction = LockAction 
  {   _updateLock' = pass
    , _lock' = lock
  }
lock :: GameStateExceptT ()
lock = updateEnvironmentM msg 
  where 
    msg = "You chant the magic words to do the impossible. "
            <> "And yet somehow fail to lock the floor."
openAction :: OpenAction 
openAction = OpenAction {
    _updateOpen' = pass 
  , _open' = const open 
} 

open :: GameStateExceptT ()
open = updateEnvironmentM msg 
  where 
    msg = "very funny. How do you open a floor?"
lookAction :: LookAction 
lookAction = LookAction {
    _updateLook' = pass
  , _look' = look lookAt lookOn (const (const lookIn))  
}
lookIn :: GameStateExceptT ()
lookIn = updateEnvironmentM msg
  where
    msg = "You try really hard to look in the floor, "
            <> "but your x-ray vision isn't working. "
            <> "Possibly because it doesn't exist"

lookOn :: Object -> (Map (GID Object) Container -> GameStateExceptT ())
lookOn = lookAtOpenBoxM kitchenFloorGID

lookAt :: Object -> (Map (GID Object) Container -> GameStateExceptT ())
lookAt = lookOn 

putAction :: PutAction 
putAction = PutAction {
    _updatePut' = pass 
  , _put' = const (const pass) 
}

getAction :: GetAction 
getAction = GetAction {
    _updateGet' = pass
  , _get' = getM
} 

getM :: GameStateExceptT ()
getM = updateEnvironmentM msg 
  where
    msg = "Not sure what you are trying to accomplish by getting a floor"