module HauntedHouse.Game.Actions.Get where
import HauntedHouse.Game.Model.World
import Control.Monad.Except (MonadError(..))
import HauntedHouse.Game.Model.GID (GID)
import HauntedHouse.Game.Object (getObjectM)
import Prelude hiding (fromLabel)
import HauntedHouse.Game.Engine.Utilities
import Data.These
import HauntedHouse.Game.Player (setPlayerInventoryM)
import HauntedHouse.Game.Model.Display (updatePlayerActionM)

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
    Anchoring _ -> throwError sillyMSG
  where
    alreadyHaveMSG = "Neat trick, trying tp get something you already have."
    sillyMSG = "You begin to feel a little silly trying to get that."

standardGetM :: GetInput
                  -> GameStateExceptT ()
standardGetM (GetInput rGid removedEntity rLabel (On fromGid)) = do
  let rShortName = _shortName' removedEntity
  fromEntity@(Object {..}) <- getObjectM fromGid
  nexus <- throwMaybeM notContainerMSG _mNexus'
  containment <- throwMaybeM notContainerMSG (maybeContainment nexus)
  updatedNexus <- conConstructor <$> case _unContainment' containment of
    This _ -> throwError impassMSG
    That con -> do
                  updatedCon <- removeFromShelfM rGid rLabel con
                  pure (That updatedCon)
    These cin con -> do
                       updatedCon <- removeFromShelfM rGid rLabel con
                       pure (These cin updatedCon)
  setPlayerInventoryM rGid removedEntity fromGid fromEntity updatedNexus
  updatePlayerActionM ("You get the " <> rShortName)
  where
    conConstructor = Containment' . Containment
    notContainerMSG = "standardGetM' error: fromObject not a container"
    impassMSG = "standardGetM: illogical situation "

standardGetM (GetInput rGid removedEntity rLabel (In fromGid)) = do
  fromEntity@(Object {..}) <- getObjectM fromGid
  nexus <- throwMaybeM notContainerMSG _mNexus'
  containment <- throwMaybeM notContainerMSG (maybeContainment nexus)
  updatedNexus <- conConstructor <$> case _unContainment' containment of
    This cin -> do 
                updatedCin <- removeFromContainedInM rGid rLabel cin
                pure (This updatedCin)
    That _ -> throwError impassMSG 
    These cin con -> do
                       updatedCin <- removeFromContainedInM rGid rLabel cin
                       pure (These updatedCin con)
  setPlayerInventoryM rGid removedEntity fromGid fromEntity updatedNexus
  where
    conConstructor = Containment' . Containment
    notContainerMSG = "standardGetM' error: fromObject not a container"
    impassMSG = "standardGetM: illogical situation "
