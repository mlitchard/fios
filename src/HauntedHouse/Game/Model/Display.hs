{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module HauntedHouse.Game.Model.Display where

import Data.Map.Strict qualified (elems, toList, size)

import HauntedHouse.Game.Model.World

import HauntedHouse.Game.Model.Mapping (ContainerMap(..), Label (..))
import HauntedHouse.Game.Object (getShortNameM, setObjectMapM)

import Data.List.NonEmpty ( (<|), reverse, toList, singleton )

import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Condition (Proximity (..), Perceptibility (..))
import Control.Monad.Except (MonadError(..))
import Data.These (These(..))
import qualified Data.Text
import qualified Data.List
import HauntedHouse.Game.World (getExitM)
import Data.Text (toLower)
import HauntedHouse.Tokenizer.Data (Lexeme(..))
import HauntedHouse.Recognizer (Preposition)
import qualified Data.Either.Combinators

updatePlayerActionM :: Text -> GameStateExceptT ()
updatePlayerActionM action = do
  print ("Player action " <> action)
  narration@(Narration {..}) <- _narration' <$> get
  let updatedAction = action <| _playerAction'
  modify' (\gs -> gs{_narration' = narration{_playerAction' = updatedAction}})

finalizePlayerActionM :: GameStateExceptT ()
finalizePlayerActionM = do
  narration@(Narration playerAction _ _ _) <- _narration' <$> get
  let flippedAction = Data.List.NonEmpty.reverse playerAction
  modify' (\gs -> gs{_narration' = narration{_playerAction' = flippedAction}})

-- FIXME - residual empty list
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

updateContainerDescriptionM :: Preposition
                                -> (GID Object,Object)
                                -> GameStateExceptT ()
updateContainerDescriptionM prep (gid,entity) = do
  nexus <- throwMaybeM notContainerMSG (_mNexus' entity)
  case nexus of
    (Containment' (Containment containment)) -> caseContainement containment
    _ -> throwError notContainerMSG
  where
    caseContainement containment =
      case containment of
        (This containedIn) -> cIn containedIn >>= updateNexusM
        (That _) -> cOn
        (These _ _) -> cInOn

    updateNexusM :: Nexus -> GameStateExceptT ()
    updateNexusM nexus = do
      setObjectMapM gid (entity {_mNexus' = Just nexus})

    notContainerMSG = _shortName' entity <> "isn't a container. Fix your shit"
    cOn = throwError "cOn not implemented"
    cInOn = throwError "cInOn not implemented"

    cIn :: ContainedIn -> GameStateExceptT Nexus
    cIn  (ContainedIn containerInterface cmap) = do
      descriptionList <- makeDescriptionListM cmap
      description <- case prep of
                AT -> pure $ openSeeShallow descriptionList
                IN -> pure $ openSeeDeep descriptionList
                _  -> throwError nonsenseMSG
      let updatedCInterface = containerInterface{_describe' = description}
          container = Containment (This (ContainedIn updatedCInterface cmap))
      pure (Containment' container)
      where
        nonsenseMSG :: Text
        nonsenseMSG = "Nonsense Detected: updateContainerDescriptionM "
                        <> show prep

-- FIXME perception test should happen seperately
describeObjectM :: Object -> GameStateExceptT ()
describeObjectM (Object shortName _ desc _ _ percept orientation mNexus _) = do
  case percept of
    Imperceptible -> throwError "You don't see that."
    Perceptible -> updatePlayerActionM success
                    >> describeOrientationM preamble orientation
                    >> mapM_ updateEnvironmentM desc
                    >> maybeDescribeNexusM mNexus
  where
    success = "You look at the " <> shortName
    preamble = "The " <> shortName <> " is "

describeInventoryM :: GameStateExceptT ()
describeInventoryM = do
  inventory <- _p_inv' . _player' <$> get
  case inventory of 
    [] -> updateEnvironmentM "You are empty-handed"
    gids -> do
              inv <- mapM getShortNameM gids
              updateEnvironmentM "You are carrying:"
              mapM_ updateEnvironmentM inv
                  
describeOrientationM :: Text -> Orientation -> GameStateExceptT ()
describeOrientationM preamble orientation = do
  desc <- case orientation of
            ContainedBy' containedBy -> describeContainedByM containedBy
            Inventory -> pure "in your inventory."
            (Floor _) -> pure "on the floor."
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
describeProximity PlacedBehind = " behind "

-- FIXME: unsafe
describeAnchoring :: RoomAnchor -> Text
describeAnchoring roomAnchor = "In the " <> anchor
  where
    anchor = Data.Text.toLower
              $ Data.List.head
              $ Data.Text.splitOn "Anchor"
              $ toText roomAnchor

maybeDescribeNexusM :: Maybe Nexus -> GameStateExceptT ()
maybeDescribeNexusM Nothing = pass
maybeDescribeNexusM (Just nexus) =
  case nexus of
    (Containment' containment) -> describeContainmentM containment
    (Portal' portal) ->  describePortalM portal
    (Door' door)       -> describeDoorM door

describeDoorM :: Door -> GameStateExceptT ()
describeDoorM (Door interface _) = do
  updateEnvironmentM openStateMSG
  where
    openState = _openState' interface
    openStateMSG = "The door is " <> toLower (show openState)
{-
describePortalM :: Portal -> GameStateExceptT ()
describePortalM (Portal _ gid) = do
  exit <- _title' <$> (getLocationM . _toDestination' =<< getExitM gid)
  updateEnvironmentM ("an exit leading to " <> exit)
-}
--  either describeContainmentM describePortalM nexus

describeContainmentM :: Containment -> GameStateExceptT ()
describeContainmentM (Containment (This containedIn)) =
  describeContainedInM containedIn
describeContainmentM (Containment (That containedOn)) =
  describeContainedOnM containedOn
describeContainmentM (Containment (These containedIn containedOn)) =
  describeContainedInM containedIn >> describeContainedOnM containedOn

describeContainedInM :: ContainedIn -> GameStateExceptT ()
describeContainedInM (ContainedIn interface _) = do
  describeOpenStateM (_openState' interface) (_describe' interface)

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

describeOpenStateM :: OpenState -> Text -> GameStateExceptT ()
describeOpenStateM openState describe = do
  case openState of
    Open   -> do
                updateEnvironmentM isOpenMsg
                updateEnvironmentM describe
    Closed -> updateEnvironmentM isClosed
  where
    isClosed = "It's closed"
    isOpenMsg = "It's open"

openSeeDeep :: Maybe (NonEmpty (Label Object, NonEmpty Text)) -> Text
openSeeDeep (Just cList) = unlines $ fmap examineObjectsInside (Data.List.NonEmpty.toList cList)
openSeeDeep Nothing = "It's empty"

howMany :: Int -> Text
howMany 1 = "a"
howMany 2 = "two"
howMany 3 = "three"
howMany _ = "several"

examineObjectsInside :: (Label Object, NonEmpty Text) -> Text
examineObjectsInside (Label name, shortDescList) = preamble <> details
  where
    objectAmount = length shortDescList
    objectAmountText = howMany objectAmount
    preamble
      | objectAmount > 1 = pluralSeen
      | otherwise = "you see a "
    pluralSeen = "you see "
                  <> objectAmountText
                  <> " "
                  <> (toLower . show $ name) <> "\n"
    details = unlines $ Data.List.NonEmpty.toList shortDescList

makeDescriptionListM :: ContainerMap Object
                          -> GameStateExceptT
                              (Maybe (NonEmpty (Label Object, NonEmpty Text)))
makeDescriptionListM (ContainerMap cmap) = do
  case cList' of
    Just cList -> Just <$> mapM makeShortNameListM cList
    Nothing -> pure Nothing
  where
    makeShortNameListM :: (Label Object, NonEmpty (GID Object))
                          -> GameStateExceptT (Label Object,NonEmpty Text)
    makeShortNameListM (label, gids) = do
      shortNames <- mapM getShortNameM gids
      pure (label, shortNames)
    cList' = nonEmpty $ Data.Map.Strict.toList cmap

openSeeShallow :: Maybe (NonEmpty (Label Object, NonEmpty Text)) -> Text
openSeeShallow (Just _) = "you see something inside"
openSeeShallow Nothing  = "You don't see anything inside"

describePortalM :: Portal -> GameStateExceptT ()
describePortalM (Portal _ gid) = do
  exit <- _title' <$> (getLocationM . _toDestination' =<< getExitM gid)
  updateEnvironmentM ("an exit leading to " <> exit)
