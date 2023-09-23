module Game.Actions.Open where

import Game.Model.Display
import Game.Model.World
import Game.Model.GID (GID)
import Game.Object (getObjectM, setObjectMapM, togglePerceptabilityM)
import Control.Monad.Except (MonadError(..))
import Data.These (These(..))

alreadyOpenM :: GameStateExceptT () 
alreadyOpenM = do 
  updateEnvironmentM "That's already open"

nonsenseOpenM :: Object -> GameStateExceptT () 
nonsenseOpenM (Object {..}) = do 
  updateEnvironmentM ("You can't open a " <> _shortName')
   
standardOpenM :: Object -> GameStateExceptT ()
standardOpenM _ = pass
  

        
{-
openContainerM :: Container -> GameStateExceptT Nexus
openContainerM (Container containment) = do
  updatedContainer <-  case containment of
    (This containerIn) -> do
                            contIn' <- makeOpenContainerM containerIn
                            togglePerceptabilityM Open contIn'
                            pure (This contIn') 
    (That _) -> throwError "You can't open that."
    (These containerIn containerOn ) -> do
                                          containerIn' <-
                                                makeOpenContainerM containerIn
                                          pure $ These containerIn' containerOn
  pure $ Container' (Container updatedContainer)

makeOpenContainerM :: ContainedIn -> GameStateExceptT ContainedIn
makeOpenContainerM containedIn@(ContainedIn {..}) = do
  if (_openState == Open)
    then throwError "It's already open"
  throwMaybeM "It's already open." $ makeOpenThingM _openState' containedIn 
  
openDoorM :: Door -> GameStateExceptT Nexus
openDoorM (Door interface exitGID) = do
  openInterface <- makeOpenThingM (_openState' interface) interface
  pure $ Door' (Door openInterface exitGID)
  -}
