module HauntedHouse.Game.World where

import qualified Data.List.NonEmpty
import qualified Data.Map.Strict (insert, lookup, insertWith, Map)

import HauntedHouse.Game.Model ( GameStateExceptT, GameState (_world') )
import HauntedHouse.Game.Model.World (Exit (..), World (..), Object (..)
        , Location (..), ExitGIDMap (..), Portal (..))
import HauntedHouse.Game.Model.Mapping
    ( GIDToDataMapping(GIDToDataMapping, _unGIDToDataMapping'),
      LabelToGIDListMapping(..),Label, LabelToGIDMapping (..))
import HauntedHouse.Internal ( throwMaybeM, throwEitherM )
import HauntedHouse.Game.Location (getLocationIdM, getLocationM)
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Object (getObjectM)

setWorldExitMapM :: GID Exit -> Exit -> GameStateExceptT ()
setWorldExitMapM gid exit = do
  world <- _world' <$> get
  let updated = GIDToDataMapping $ Data.Map.Strict.insert gid exit
                  $ (_unGIDToDataMapping' . _exitMap') world
  modify' (\gs -> gs {_world' = world {_exitMap' = updated}})

getExitM :: GID Exit -> GameStateExceptT Exit
getExitM gid = do
  throwLookup . _unGIDToDataMapping' . _exitMap' . _world' =<< get
  where
    throwLookup :: Data.Map.Strict.Map (GID Exit) Exit -> GameStateExceptT Exit
    throwLookup exitMap = throwMaybeM lookupErr $ Data.Map.Strict.lookup gid exitMap
    lookupErr = "Not found: exit gid " <> show gid

getExitObjectM :: Label Exit -> GameStateExceptT Exit
getExitObjectM exitLabel = do
  (ExitGIDMap (LabelToGIDMapping objGIDMap)) <- (throwMaybeM dirErr . _directions')
                                                  =<< (getLocationM =<< getLocationIdM)
  objGID <- throwMaybeM noExit $ Data.Map.Strict.lookup exitLabel objGIDMap
  getExitFromObjectM objGID
    where
      noExit = "Error: Exit does not exist " <> show exitLabel
      dirErr = "Error: No Exits"

getExitFromObjectM :: GID Object -> GameStateExceptT Exit
getExitFromObjectM objectGID =
  getContainment 
    >>= throwMaybeM notContainer 
    >>= throwEitherM notExit 
    <&> _portalExit'
    >>= getExitM
  where
    getContainment = _containment' <$> getObjectM objectGID
    notContainer = "Error: Not Container - " <> show objectGID
    notExit = "Error: Not Exit - " <> show objectGID


{-
getExitObjectM :: Label Exit -> GameStateExceptT Object
getExitObjectM exitLabel = do
  exitMap <- _unGIDToDataMapping' . _exitMap' ._world' <$> get
  exitGID :: GID Object <- throwExitGID . _unExitGIDMap' 
                =<< (throwMaybeM directionErr . _directions'
                =<< (getLocationM =<< getLocationIdM))
  throwMaybeM attemptFailedMSG 
    $ Data.Map.Strict.lookup exitGID exitMap
    where
      attemptFailedMSG = "You can't go that way"

      throwExitGID :: LabelToGIDMapping Exit Object -> GameStateExceptT (GID Exit)
      throwExitGID ltgMapping = throwMaybeM exitGIDErr
        $ Data.Map.Strict.lookup exitLabel
        $ _unLabelToGIDMapping' ltgMapping

      _getExitGIDMap = throwMaybeM exitGIDMapErr . _directions'
                                =<< (getLocationM =<< getLocationIdM)
                                
      directionErr = "direction map does not exist"
      exitGIDMapErr = "ExitGIDmap does not exist "
      exitGIDErr = ("Could not find Exit GID from Exit label " :: Text) 
                    <> show exitLabel
-}
{-

newtype LabelToGIDMapping a b
  = LabelToGIDMapping
      { _unlabelToGIDListMapping :: Data.Map.Strict.Map (Label a) (GID b)}
        deriving stock Show 

newtype GIDToDataMapping a 
  = GIDToDataMapping {_unGIDToDataMapping' :: Data.Map.Strict.Map (GID a) a} 
      deriving stock Show

-}
getGIDListM :: Label Object -> GameStateExceptT (NonEmpty (GID Object))
getGIDListM label = do
  -- throwMaybe objErr . (Data.Map.Strict.lookup gid <$> unwrappedMap) =<< get
  throwMaybeM labelErr . (Data.Map.Strict.lookup label <$> unwrappedMap) =<< get
  where
    labelErr = "Could not find that gid list based on label " <> show label
    unwrappedMap = _unLabelToGIDListMapping' . _objectLabelMap' . _world'

setObjectLabelMapM :: Label Object -> GID Object -> GameStateExceptT ()
setObjectLabelMapM label gid = do
  world <- _world' <$> get
  let objectLabelMap = _objectLabelMap' world
      labelToGIDListMap  = LabelToGIDListMapping
                          . Data.Map.Strict.insertWith (<>) label gid'
                          $ _unLabelToGIDListMapping' objectLabelMap
  modify' (\gs -> gs{_world' = world {_objectLabelMap' = labelToGIDListMap}})
  where
    gid' = Data.List.NonEmpty.singleton gid
