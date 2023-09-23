module Game.Actions.Get where
import Game.Model.World
import Control.Monad.Except (MonadError(..))
import Game.Model.GID (GID)
import Game.Object (getObjectM)
import Prelude hiding (fromLabel)
import Game.Engine.Utilities
import Data.These
import Game.Player (setPlayerInventoryM)
import Game.Model.Display (updatePlayerActionM)

noGetM :: GameStateExceptT ()
noGetM = throwError "Are you serious?"

tryGetM :: GID Object 
            -> (GetInput -> GameStateExceptT ()) 
            -> Object 
            -> GameStateExceptT ()
tryGetM gid getFunction entity@(Object{..}) = do 
  let getInput onOrIn = 
        GetInput { 
          _removedEntityGID' = gid
          , _removedEntity' = entity
          ,_removedEntityLabel' = _entityLabel' 
          ,_entityOnOrIn' = onOrIn 
        }  
  case _orientation' of
    ContainedBy' (ContainedBy cBy _) -> getFunction (getInput cBy)
    Inventory -> throwError alreadyHaveMSG
    Floor fromGid -> getFunction (getInput (On fromGid))
    AnchoredTo' (fromGid, _) -> getFunction (getInput (On fromGid))
    Anchored _ -> throwError sillyMSG
  where
    alreadyHaveMSG = "Neat trick, trying tp get something you already have."
    sillyMSG = "You begin to feel a little silly trying to get that."

standardGetM :: GetInput
                  -> GameStateExceptT ()
standardGetM _ = pass  
                  
