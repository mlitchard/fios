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
    Containment' containment -> openContainerM containment
    Door' door              -> openDoorM door
    _                       -> throwError notContainerMSG
  let updatedEntity = entity{_mNexus' = Just updatedNexus}
  setObjectMapM gid updatedEntity
  maybeDescribeNexusM (_mNexus' updatedEntity)
  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
  where
    notContainerMSG = "You can't open that."

openContainerM :: Containment -> GameStateExceptT Nexus
openContainerM (Containment containment) = do
  updatedContainment <-  case containment of
    (This containerIn) -> This <$> makeOpenContainerM containerIn
    (That _) -> throwError "You can't open that."
    (These containerIn containerOn ) -> do
                                          containerIn' <-
                                                makeOpenContainerM containerIn
                                          pure $ These containerIn' containerOn
  pure $ Containment' (Containment updatedContainment)

makeOpenContainerM :: ContainedIn -> GameStateExceptT ContainedIn
makeOpenContainerM (ContainedIn interface contents) = do 
  openInterface <- makeOpenThingM (_openState' interface) interface
  pure $ ContainedIn openInterface contents
  
openDoorM :: Door -> GameStateExceptT Nexus
openDoorM (Door interface exitGID) = do
  openInterface <- makeOpenThingM (_openState' interface) interface
  pure $ Door' (Door openInterface exitGID)
  
makeOpenThingM :: OpenState 
                  -> ContainerInterface 
                  -> GameStateExceptT ContainerInterface
makeOpenThingM openState interface = case openState of 
  Closed -> pure $ interface {_openState' = Open}
  Open ->  throwError "It's already open"
