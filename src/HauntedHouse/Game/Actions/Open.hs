module HauntedHouse.Game.Actions.Open where

import HauntedHouse.Game.Model.Display
import HauntedHouse.Game.Model.World
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Object (getObjectM, setObjectMapM)
import Control.Monad.Except (MonadError(..))
import Data.These (These(..))

standardOpenM :: GID Object -> GameStateExceptT ()
standardOpenM gid = do
  entity <- getObjectM gid
  nexus <- throwMaybeM notContainerMSG (_mNexus' entity)
  updatedNexus <- case nexus of
    Containment' containment -> openContainer containment
    Door' door              -> openDoor door
    _                       -> throwError notContainerMSG
  let updatedEntity = entity{_mNexus' = Just updatedNexus}
  setObjectMapM gid updatedEntity 
  maybeDescribeNexusM (_mNexus' updatedEntity)
  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
  where
    notContainerMSG = "You can't open that."

openContainer :: Containment -> GameStateExceptT Nexus
openContainer (Containment containment) = do
  updatedContainment <- throwMaybeM openErr $ case containment of
    (This containerIn) -> Just $ This (makeOpenContainer containerIn)
    (That _) -> Nothing -- throwError "You can't open that."
    (These containerIn containerOn ) -> do 
                                          let containerIn' = 
                                                makeOpenContainer containerIn
                                          Just $ These containerIn' containerOn
  pure $ Containment' (Containment updatedContainment)
  where
    openErr = "You can't open that." 

makeOpenContainer :: ContainedIn -> ContainedIn
makeOpenContainer (ContainedIn interface contents) = 
  ContainedIn (interface {_openState' = Open}) contents

openDoor :: Door -> GameStateExceptT Nexus
openDoor (Door doorInterface exitGID) = do
  pure $ Door' (Door (doorInterface {_openState' = Open}) exitGID)

  -- let nexus = ContainerInterface' updatedContainerInterface
