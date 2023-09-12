module HauntedHouse.Game.Actions.Close where 
import HauntedHouse.Game.Model.World 
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Object (getObjectM, setObjectMapM)
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Game.Model.Display (maybeDescribeNexusM, updateDisplayActionM, showPlayerActionM, showEnvironmentM)
import Data.These

standardCloseM :: GID Object -> GameStateExceptT () 
standardCloseM gid = do
  entity <- getObjectM gid
  nexus <- throwMaybeM notContainerMSG (_mNexus' entity) 
  updatedNexus <- case nexus of
    Containment' containment -> closeContainerM containment
    Door' door              -> closeDoorM door
    _                       -> throwError notContainerMSG
  let updatedEntity = entity{_mNexus' = Just updatedNexus}
  setObjectMapM gid updatedEntity
  maybeDescribeNexusM (_mNexus' updatedEntity)
  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
  where
    notContainerMSG = "you can't close that" 

closeContainerM :: Containment -> GameStateExceptT Nexus
closeContainerM (Containment containment) = do
  updatedContainment <-  case containment of
    (This containerIn) -> This <$> makeClosedContainerM containerIn
    (That _) -> throwError "You can't close that."
    (These containerIn containerOn ) -> do
                                          containerIn' <-
                                            makeClosedContainerM containerIn
                                          pure $ These containerIn' containerOn
  pure $ Containment' (Containment updatedContainment)

makeClosedContainerM :: ContainedIn -> GameStateExceptT ContainedIn
makeClosedContainerM (ContainedIn interface contents) = do 
  openInterface <- makeClosedThingM (_openState' interface) interface
  pure $ ContainedIn openInterface contents

closeDoorM :: Door -> GameStateExceptT Nexus
closeDoorM (Door interface exitGID) = do
  openInterface <- makeClosedThingM (_openState' interface) interface
  pure $ Door' (Door openInterface exitGID)

makeClosedThingM :: OpenState 
                  -> ContainerInterface 
                  -> GameStateExceptT ContainerInterface
makeClosedThingM openState interface = case openState of 
  Open -> throwError "It's already closed." 
  Closed -> pure $ interface {_openState' = Closed} 