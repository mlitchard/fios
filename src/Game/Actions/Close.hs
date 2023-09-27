module Game.Actions.Close where 
import Game.Model.World 
import Game.Model.GID (GID)
import Game.Object (getObjectM, setObjectMapM)
import Control.Monad.Except (MonadError(..))
import Game.Model.Display (updateDisplayActionM, showPlayerActionM, showEnvironmentM)
import Data.These

standardCloseM :: GID Object -> GameStateExceptT () 
standardCloseM _gid = pass
{-
  entity <- getObjectM gid
  nexus <- throwMaybeM notContainerMSG (_mNexus' entity) 
  updatedNexus <- case nexus of
    Container' containment -> closeContainerM containment
    Door' door              -> closeDoorM door
    _                       -> throwError notContainerMSG
  let updatedEntity = entity{_mNexus' = Just updatedNexus}
  setObjectMapM gid updatedEntity
  maybeDescribeNexusM (_mNexus' updatedEntity)
  updateDisplayActionM (showPlayerActionM >> showEnvironmentM)
  where
    notContainerMSG = "you can't close that" 

closeContainerM :: Container -> GameStateExceptT Nexus
closeContainerM (Container containment) = do
  updatedContainer <-  case containment of
    (This contIn) -> do
                       contIn' <- makeClosedContainerM contIn
                       togglePerceptabilityM Closed contIn'
                       pure (This contIn') 
    (That _) -> throwError "You can't close that."
    (These contIn contOn ) -> do
                                contIn' <- makeClosedContainerM contIn
                                togglePerceptabilityM Closed contIn 
                                pure $ These contIn' contOn
  pure $ Container' (Container updatedContainer)

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
  Closed -> throwError "It's already closed." 
  Open -> pure $ interface {_openState' = Closed} 
  -}