{-# OPTIONS_GHC -Wno-orphans #-}
module HauntedHouse.Game.Model.Display where

import Data.Map.Strict qualified (null, elems, toList)

import HauntedHouse.Game.Model.World
        
import HauntedHouse.Game.Model.Mapping (ContainerMap(..), NeighborMap (..))
import HauntedHouse.Game.Object (getObjectM)

import Data.List.NonEmpty ( (<|), reverse )

import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Model.Condition (Proximity, Perceptibility (..))
import Control.Monad.Except (MonadError(..))
import Data.These (These(..))

updatePlayerActionM :: Text -> GameStateExceptT ()
updatePlayerActionM action = do
  narration@(Narration playerAction _ _ _) <- _narration' <$> get
  let updatedAction = action <| playerAction 
  modify' (\gs -> gs{_narration' = narration{_playerAction' = updatedAction}}) 

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
 
{-

data Narration = Narration {
      _playerAction' :: Data.List.NonEmpty.NonEmpty Text
    , _enviroment'   :: Data.List.NonEmpty.NonEmpty Text
    , _npcResponse' :: Data.List.NonEmpty.NonEmpty Text
    , _scene'       :: Scene
  } deriving stock Show

data Object = Object {
    _shortName'       :: Text
  , _odescription'    :: [Text]
  , _descriptives'    :: [Label Adjective]
  , _moveability'     :: Moveability
  , _perceptability'  :: Perceptibility
  , _mNexus'          :: Maybe Nexus
}

-}
describeObjectM :: GID Object -> GameStateExceptT ()
describeObjectM gid = do
  (Object shortName desc _ _ percept mNexus) <- getObjectM gid
  let success = "You look at the " <> shortName
  case percept of
    Imperceptible -> throwError "You don't see that."
    Perceptible -> updatePlayerActionM success 
                    >> mapM_ updateEnvironmentM desc 
                    >> maybeNexusM mNexus
                    >> finalizeEnvironmentM   

maybeNexusM :: Maybe Nexus -> GameStateExceptT () 
maybeNexusM Nothing = pass 
maybeNexusM (Just (Nexus nexus)) = 
  either describeContainment describePortal nexus

{-
newtype Containment = Containment
  { _unContainment' :: These ContainedIn
                        (Either ContainedOn ContainedBy)
  } deriving stock (Show)
-}

describeContainment :: Containment -> GameStateExceptT () 
describeContainment (Containment (This (ContainedIn _ _))) = pass
describeContainment (Containment (That (Left _))) = pass 
describeContainment (Containment (That (Right _))) = pass 
describeContainment (Containment (These _ _)) = pass

{-
data Portal = Portal {
      _portalInterface' :: Interface
    , _portalExit' :: GID Exit
  } deriving stock Show
-}
describePortal :: Portal -> GameStateExceptT ()
describePortal (Portal interface' portalExit) = pass
 

