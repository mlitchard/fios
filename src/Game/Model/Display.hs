{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Game.Model.Display where

import Data.Map.Strict qualified (member, lookup)
import Game.Model.World
import Game.Object (getShortNameM)
import Data.List.NonEmpty ( (<|), reverse, toList, singleton )
import Game.Model.GID (GID)
import Game.Model.Condition (Proximity (..))
import qualified Data.Text
import qualified Data.List
import Game.Model.Mapping
import Data.Text (toLower)

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

-- FIXME perception test should happen seperately
describeObjectM :: GID Object -> GameStateExceptT ()
describeObjectM gid = error "undefined" {- do
  (Object {..}) <- getObjectM gid
  let success = "You look at the " <> _shortName'
      preamble = "The " <> _shortName' <> " is "
-- describeObjectM (Object shortName _ desc _ _ percept orientation _) = do
  case _perceptability' of
    Imperceptible -> throwError "You don't see that."
    Perceptible -> updatePlayerActionM success
                    >> describeOrientationM preamble _orientation'
                    >> mapM_ updateEnvironmentM _odescription'
                    >> maybeDescribeContainerShallowM gid
-}
isContainerM :: GID Object -> GameStateExceptT Bool
isContainerM gid = do
  res <- _unGIDToDataMap' . _containerMap' . _world' <$> get
  pure $ Data.Map.Strict.member gid res

maybeDescribeContainerShallowM :: GID Object -> GameStateExceptT () 
maybeDescribeContainerShallowM gid = do 
  res <- _unGIDToDataMap' . _containerMap' . _world' <$> get
  case Data.Map.Strict.lookup gid res of 
    Nothing -> pass 
    Just (Container container) -> if null container 
                                    then updateEnvironmentM emsg 
                                    else updateEnvironmentM smsg
  where 
    emsg = "It's empty."
    smsg = "You see something inside." 
    
describeInventoryM :: GameStateExceptT ()
describeInventoryM = do
  inventory <- _p_inv' . _player' <$> get
  case inventory of
    [] -> updateEnvironmentM "You are empty-handed"
    gids -> do
              inv <- mapM getShortNameM gids
              updateEnvironmentM "You are carrying:"
              mapM_ updateEnvironmentM inv

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
describeProximity PlacedIn = "inside"

-- FIXME: unsafe
describeAnchoring :: RoomSection -> Text
describeAnchoring roomAnchor = "In the " <> anchor
  where
    anchor = Data.Text.toLower
              $ Data.List.head
              $ Data.Text.splitOn "Anchor"
              $ toText roomAnchor

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

openSeeShallow :: Maybe (NonEmpty (Label Object, NonEmpty Text)) -> Text
openSeeShallow (Just _) = "you see something inside"
openSeeShallow Nothing  = "You don't see anything inside"
