{-# OPTIONS_GHC -Wno-orphans #-}
module HauntedHouse.Game.Model.Display where

import Data.Map.Strict qualified (null, elems, toList, size)

import HauntedHouse.Game.Model.World

import HauntedHouse.Game.Model.Mapping (ContainerMap(..), NeighborMap (..))
import HauntedHouse.Game.Object (getObjectM, getShortNameM)

import Data.List.NonEmpty ( (<|), reverse, toList, singleton )

import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Condition (Proximity (..), Perceptibility (..))
import Control.Monad.Except (MonadError(..))
import Data.These (These(..))
import qualified Data.Text
import qualified Data.List
import HauntedHouse.Game.World (getExitM)

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

describeObjectM :: Object -> GameStateExceptT ()
describeObjectM (Object shortName desc _ _ percept orientation mNexus) = do
  case percept of
    Imperceptible -> throwError "You don't see that."
    Perceptible -> updatePlayerActionM success
                    >> describeOrientationM preamble orientation
                    >> mapM_ updateEnvironmentM desc
                    >> maybeNexusM mNexus
  where
    success = "You look at the " <> shortName
    preamble = "The " <> shortName <> " is "

describeOrientationM :: Text -> Orientation -> GameStateExceptT ()
describeOrientationM preamble orientation = do
  desc <- case orientation of
            ContainedBy' containedBy -> describeContainedByM containedBy
            Inventory -> pure "in your inventory."
            Floor     -> pure "on the floor."
            (AnchoredTo' anchoredTo) -> describeAnchoredToM anchoredTo
            Anchoring roomAnchor -> pure $ describeAnchoring roomAnchor
  updateEnvironmentM (preamble <> desc)

describeContainedByM :: ContainedBy -> GameStateExceptT Text
describeContainedByM (ContainedBy onOrIn _) = do
  (prep,shortName) <- case onOrIn of
                        (On gid) -> do
                                       shortName <- getShortNameM gid
                                       pure ("on", shortName)
                        (In gid) -> do
                                       shortName <- getShortNameM gid
                                       pure ("in", shortName)
  pure (prep <> " the " <> shortName)

describeAnchoredToM :: (GID Object, Proximity) -> GameStateExceptT Text
describeAnchoredToM (gid, proximity) = do
  shortName <- getShortNameM gid
  pure (proximity' <> shortName)
  where
    proximity' = describeProximity proximity

describeProximity :: Proximity -> Text
describeProximity PlacedOn = " on "
describeProximity PlacedUnder = " under "
describeProximity PlacedAbove = " above "
describeProximity PlacedLeft = " to the left of "
describeProximity PlacedRight = " to the right of "
describeProximity PlacedFront = " in front of "
describeProximity PlacedBack = " behind "

-- FIXME: unsafe
describeAnchoring :: RoomAnchor -> Text
describeAnchoring roomAnchor = "In the " <> anchor
  where
    anchor = Data.Text.toLower
              $ Data.List.head
              $ Data.Text.splitOn "Anchor"
              $ toText roomAnchor

maybeNexusM :: Maybe Nexus -> GameStateExceptT ()
maybeNexusM Nothing = pass
maybeNexusM (Just (Nexus nexus)) =
  either describeContainmentM describePortalM nexus

describeContainmentM :: Containment -> GameStateExceptT ()
describeContainmentM (Containment (This containedIn)) =
  describeContainedInM containedIn
describeContainmentM (Containment (That containedOn)) =
  describeContainedOnM containedOn
describeContainmentM (Containment (These containedIn containedOn)) =
  describeContainedInM containedIn >> describeContainedOnM containedOn

describeContainedInM :: ContainedIn -> GameStateExceptT ()
describeContainedInM (ContainedIn interface cmap) = do
  case interface of
    (ContainerInterface' cInterface) -> describeOpenStateM
                                          (_openState' cInterface)
                                          cmap
    PortalInterface -> do
      updateEnvironmentM "This container has an unusual opening"

describeContainedOnM :: ContainedOn -> GameStateExceptT ()
describeContainedOnM (ContainedOn (ContainerMap cmap)) = do
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

describeOpenStateM :: OpenState -> ContainerMap Object -> GameStateExceptT ()
describeOpenStateM openState (ContainerMap cmap ) = do
  case openState of
    Open   -> updateEnvironmentM isOpenMsg >> updateEnvironmentM openSee
    Closed -> updateEnvironmentM isClosed
  where
    isClosed = "It's closed"
    isOpenMsg = "It's open"
    openSee
      | Data.Map.Strict.null cmap = "You don't see anything inside"
      | otherwise  = "You see something inside"

describePortalM :: Portal -> GameStateExceptT ()
describePortalM (Portal _ gid) = do
  exit <- _title' <$> (getLocationM . _toDestination' =<< getExitM gid)
  updateEnvironmentM ("an exit leading to " <> exit)
