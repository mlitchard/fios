{-# OPTIONS_GHC -Wno-orphans #-}
module HauntedHouse.Game.Model.Display where

import Data.Map.Strict qualified (null, elems, toList, size)

import HauntedHouse.Game.Model.World

import HauntedHouse.Game.Model.Mapping (ContainerMap(..), NeighborMap (..))
import HauntedHouse.Game.Object (getObjectM, getShortNameM)

import Data.List.NonEmpty ( (<|), reverse, toList, singleton )

import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Condition (Proximity, Perceptibility (..))
import Control.Monad.Except (MonadError(..))
import Data.These (These(..))

updatePlayerActionM :: Text -> GameStateExceptT ()
updatePlayerActionM action = do
  narration@(Narration playerAction _ _ _) <- _narration' <$> get
  let updatedAction = action <| playerAction
  modify' (\gs -> gs{_narration' = narration{_playerAction' = updatedAction}})

finalizePlayerActionM :: GameStateExceptT ()
finalizePlayerActionM = do 
  narration@(Narration playerAction _ _ _) <- _narration' <$> get
  let flippedAction = Data.List.NonEmpty.reverse playerAction 
  modify' (\gs -> gs{_narration' = narration{_playerAction' = flippedAction}})

updateEnvironmentM :: Text -> GameStateExceptT ()
updateEnvironmentM result = do
  narration@(Narration _ env _ _) <- _narration' <$> get
  let updatedEnv = result <| env
  modify' (\gs -> gs{_narration' = narration{_enviroment' = updatedEnv}})

finalizeEnvironmentM :: GameStateExceptT ()
finalizeEnvironmentM = do
  narration@(Narration _ env _ _) <- _narration' <$> get
  let flippedEnv = Data.List.NonEmpty.reverse env
  modify' (\gs -> gs{_narration' = narration{_enviroment' = flippedEnv}})

showEnvironmentM :: GameStateExceptT ()
showEnvironmentM = do
  finalizeEnvironmentM
  environment <- _enviroment' . _narration' <$> get
  mapM_ print environment 
  clearEnvironmentM

{-
data Narration = Narration {
      _playerAction' :: Data.List.NonEmpty.NonEmpty Text
    , _enviroment'   :: Data.List.NonEmpty.NonEmpty Text
    , _npcResponse' :: Data.List.NonEmpty.NonEmpty Text
    , _scene'       :: Scene
  } deriving stock Show
-}
clearPlayerActionM :: GameStateExceptT () 
clearPlayerActionM = do 
  narration <- _narration' <$> get
  modify' (\gs -> gs{_narration' = narration{_playerAction' = clear}})
  where
    clear = Data.List.NonEmpty.singleton mempty 

clearEnvironmentM :: GameStateExceptT () 
clearEnvironmentM = do 
  narration <- _narration' <$> get
  modify' (\gs -> gs{_narration' = narration{_enviroment' = clear}})
  where
    clear = Data.List.NonEmpty.singleton mempty

showPlayerActionM :: GameStateExceptT ()
showPlayerActionM = do 
  finalizePlayerActionM
  playerAction <- _playerAction' . _narration' <$> get
  mapM_ print playerAction 
  clearPlayerActionM

updateDisplayActionM :: GameStateExceptT () -> GameStateExceptT () 
updateDisplayActionM displayAction = do 
  modify' (\gs -> gs{_displayAction'  = displayAction})

describeObjectM :: GID Object -> GameStateExceptT ()
describeObjectM gid = do
  (Object shortName desc _ _ percept orientation mNexus) <- getObjectM gid
  let success = "You look at the " <> shortName
  case percept of
    Imperceptible -> throwError "You don't see that."
    Perceptible -> updatePlayerActionM success
                    >> describeOrientation orientation 
                    >> mapM_ updateEnvironmentM desc
                    >> maybeNexusM mNexus

describeOrientation :: Orientation -> GameStateExceptT () 
describeOrientation _ = pass 
      
maybeNexusM :: Maybe Nexus -> GameStateExceptT ()
maybeNexusM Nothing = pass
maybeNexusM (Just (Nexus nexus)) =
  either describeContainmentM describePortal nexus

describeContainmentM :: Containment -> GameStateExceptT ()
describeContainmentM (Containment (This (ContainedIn interface cmap))) = do
  case interface of
    (ContainerInterface' cInterface) -> describeOpenState
                                          (_openState' cInterface)
                                          cmap
    PortalInterface -> do
      updateEnvironmentM "This container has an unusual opening"
describeContainmentM (Containment (That (ContainedOn cmap))) =
  describeContainedOn cmap
describeContainmentM (Containment (These _ _)) = pass

describeContainedOn :: ContainerMap Object -> GameStateExceptT ()
describeContainedOn (ContainerMap cmap) = do
  shortNames <- mapM getShortNameM
                  $ concatMap Data.List.NonEmpty.toList
                  $ Data.Map.Strict.elems cmap
  updateEnvironmentM preamble
  mapM_ updateEnvironmentM shortNames
  where
    preamble
      | Data.Map.Strict.size cmap == 1 = singular
      | Data.Map.Strict.size cmap > 1  = plural
      | otherwise                      = emptyContainer
    emptyContainer = "There's nothing on it"
    plural = "There's some things on it"
    singular = "There's something on it"

describeOpenState :: OpenState -> ContainerMap Object -> GameStateExceptT ()
describeOpenState openState (ContainerMap cmap ) = do
  case openState of
    Open   -> updateEnvironmentM isOpenMsg >> updateEnvironmentM openSee
    Closed -> updateEnvironmentM isClosed
  where
    isClosed = "It's closed"
    isOpenMsg = "It's open"
    openSee
      | Data.Map.Strict.null cmap = "You don't see anything inside"
      | otherwise  = "You see something inside"

describePortal :: Portal -> GameStateExceptT ()
describePortal (Portal _ _) = pass
